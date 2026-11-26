"""Word tracked changes, written directly into WordprocessingML.

python-docx cannot emit revision marks, so this edits word/document.xml with
lxml and rezips. The revision and comment markup mirrors, attribute for
attribute, the test revision already present in the source document, so the
result opens as ordinary tracked changes by the same author.

Editing model, chosen for safety rather than elegance: a replaced paragraph
KEEPS its own paragraph mark. Its runs are wrapped in <w:del> with <w:t>
rewritten to <w:delText>, and the replacement runs are appended in the same
paragraph inside <w:ins>. Accepting therefore leaves one paragraph carrying its
original style; rejecting restores the original runs exactly. Nothing has to
track paragraph-mark deletions, which is where this kind of surgery usually goes
wrong. Extra paragraphs are inserted as whole new <w:p> elements whose paragraph
mark is itself marked inserted.

British English in all generated prose.
"""
from __future__ import annotations

import copy
import re
import shutil
import zipfile
from pathlib import Path

from lxml import etree

W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
W14 = "http://schemas.microsoft.com/office/word/2010/wordml"
W15 = "http://schemas.microsoft.com/office/word/2012/wordml"
W16CID = "http://schemas.microsoft.com/office/word/2016/wordml/cid"
W16CEX = "http://schemas.microsoft.com/office/word/2018/wordml/cex"
W16DU = "http://schemas.microsoft.com/office/word/2023/wordml/word16du"
R = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
WP = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
A = "http://schemas.openxmlformats.org/drawingml/2006/main"
PIC = "http://schemas.openxmlformats.org/drawingml/2006/picture"

NS = {"w": W, "w14": W14, "w15": W15, "w16cid": W16CID, "w16cex": W16CEX,
      "w16du": W16DU, "r": R, "wp": WP, "a": A, "pic": PIC}


def q(tag: str) -> str:
    pre, local = tag.split(":")
    return "{%s}%s" % (NS[pre], local)


class TrackedDocx:
    def __init__(self, src: str, author: str = "Ricardo Twumasi",
                 initials: str = "RT", date: str = "2026-08-01T09:00:00Z",
                 date_utc: str = "2026-08-01T08:00:00Z"):
        self.src = Path(src)
        self.author, self.initials = author, initials
        self.date, self.date_utc = date, date_utc
        self.parts: dict[str, bytes] = {}
        with zipfile.ZipFile(self.src) as z:
            self.names = z.namelist()
            for n in self.names:
                self.parts[n] = z.read(n)
        self.doc = etree.fromstring(self.parts["word/document.xml"])
        self.body = self.doc.find(q("w:body"))

        ## A document that has never carried a comment has no comments parts at
        ## all. Reading them unconditionally raised KeyError on exactly the
        ## clean, authoritative files this is meant to edit, so a caller who
        ## only wanted tracked changes could not open one. The parts are created
        ## empty on demand, and _serialise() writes back only those that exist
        ## or have acquired content, so a document with no comments does not
        ## gain four empty parts and a changed content-type map.
        self._had_comments = "word/comments.xml" in self.parts
        self.comments = self._part_or_empty("word/comments.xml", "w:comments")
        self.cex = self._part_or_empty("word/commentsExtensible.xml", "w16cex:commentsExtensible")
        self.cids = self._part_or_empty("word/commentsIds.xml", "w16cid:commentsIds")
        self.cext = self._part_or_empty("word/commentsExtended.xml", "w15:commentsEx")
        self._next_id = self._max_existing_id() + 1
        self._next_comment = self._max_comment_id() + 1
        self._durable = 0x40000000
        self._media: list[tuple[str, bytes]] = []
        self.log: list[str] = []

    def _part_or_empty(self, name: str, root: str) -> etree._Element:
        if name in self.parts:
            return etree.fromstring(self.parts[name])
        nsmap = {p: NS[p] for p in ("w", "w14", "w15", "w16cid", "w16cex")}
        return etree.Element(q(root), nsmap=nsmap)

    # ---- identifiers ------------------------------------------------------
    def _max_existing_id(self) -> int:
        ids = [int(v) for v in self.doc.xpath("//@w:id", namespaces=NS)
               if str(v).lstrip("-").isdigit()]
        return max(ids) if ids else 0

    def _max_comment_id(self) -> int:
        ids = [int(c.get(q("w:id"))) for c in self.comments.findall(q("w:comment"))]
        return max(ids) if ids else -1

    def rid(self) -> int:
        self._next_id += 1
        return self._next_id

    # ---- paragraph access -------------------------------------------------
    def paragraphs(self) -> list[etree._Element]:
        """Top-level body paragraphs, in document order."""
        return [p for p in self.body if p.tag == q("w:p")]

    @staticmethod
    def para_text(p: etree._Element) -> str:
        """Visible text of a paragraph, ignoring already-deleted runs."""
        out = []
        for t in p.iter():
            if t.tag == q("w:t") and t.text:
                anc = t.getparent()
                while anc is not None and anc.tag != q("w:p"):
                    if anc.tag == q("w:del"):
                        break
                    anc = anc.getparent()
                else:
                    out.append(t.text)
        return "".join(out)

    def find_para(self, needle: str, occurrence: int = 0) -> int:
        """Index of the paragraph containing `needle`. Fails loudly."""
        norm = lambda s: re.sub(r"\s+", " ", s).strip()
        hits = [i for i, p in enumerate(self.paragraphs())
                if norm(needle).lower() in norm(self.para_text(p)).lower()]
        if not hits:
            raise LookupError(f"No paragraph contains: {needle[:80]!r}")
        if occurrence >= len(hits):
            raise LookupError(f"Only {len(hits)} paragraph(s) contain: {needle[:60]!r}")
        return hits[occurrence]

    def _style_of(self, p: etree._Element) -> etree._Element | None:
        for r in p.iter(q("w:r")):
            rpr = r.find(q("w:rPr"))
            if rpr is not None:
                return copy.deepcopy(rpr)
        return None

    # ---- building blocks --------------------------------------------------
    def _run(self, text: str, rpr: etree._Element | None) -> etree._Element:
        r = etree.Element(q("w:r"))
        if rpr is not None:
            r.append(copy.deepcopy(rpr))
        t = etree.SubElement(r, q("w:t"))
        t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
        t.text = text
        return r

    def _ins(self, children: list[etree._Element]) -> etree._Element:
        el = etree.Element(q("w:ins"))
        el.set(q("w:id"), str(self.rid()))
        el.set(q("w:author"), self.author)
        el.set(q("w:date"), self.date)
        el.set(q("w16du:dateUtc"), self.date_utc)
        for c in children:
            el.append(c)
        return el

    def _del_wrap(self, run: etree._Element) -> etree._Element:
        """Wrap one existing run in <w:del>, rewriting w:t to w:delText."""
        el = etree.Element(q("w:del"))
        el.set(q("w:id"), str(self.rid()))
        el.set(q("w:author"), self.author)
        el.set(q("w:date"), self.date)
        el.set(q("w16du:dateUtc"), self.date_utc)
        for t in run.iter(q("w:t")):
            t.tag = q("w:delText")
        return el

    # ---- edits ------------------------------------------------------------
    def replace_paragraph(self, idx: int, new_text: str, note: str = "") -> None:
        """Delete every run of paragraph `idx` and insert `new_text` in its place."""
        p = self.paragraphs()[idx]
        rpr = self._style_of(p)
        runs = [r for r in p.iter(q("w:r"))
                if r.getparent() is not None and r.getparent().tag != q("w:del")]
        for r in runs:
            parent = r.getparent()
            pos = list(parent).index(r)
            wrapper = self._del_wrap(r)
            parent.insert(pos, wrapper)
            wrapper.append(r)
        if new_text:
            p.append(self._ins([self._run(new_text, rpr)]))
        self.log.append(f"replace para {idx}: {note or new_text[:60]}")

    def delete_paragraph_text(self, idx: int, note: str = "") -> None:
        self.replace_paragraph(idx, "", note=note or "deleted")

    def insert_text_after(self, idx: int, anchor: str, new: str, note: str = "") -> None:
        """Insert `new` immediately after `anchor`, deleting nothing.

        Use this wherever a sentence is being ADDED. replace_text(i, x, x + y)
        expresses the same intent but deletes `x` and retypes it, which produces
        a <w:del> holding text identical to the <w:ins> beside it. Word renders
        that correctly and it accepts correctly, but where the anchor already
        sits inside an unaccepted <w:ins> the result is a deletion nested in an
        insertion, which LibreOffice draws as plain text and a reader of the
        marked-up PDF sees as the sentence appearing twice. An addition should
        look like an addition.
        """
        p = self.paragraphs()[idx]
        runs = [r for r in p.iter(q("w:r"))
                if r.getparent() is not None and r.getparent().tag != q("w:del")]
        spans, pos = [], 0
        for r in runs:
            for t in r.findall(q("w:t")):
                s = t.text or ""
                spans.append((pos, pos + len(s), r, t))
                pos += len(s)
        whole = "".join((t.text or "") for _, _, _, t in spans)

        end = whole.find(anchor)
        if end < 0:
            norm = lambda s: re.sub(r"\s+", " ", s)
            m = re.search(re.escape(norm(anchor)).replace(r"\ ", r"\s+"), whole)
            if not m:
                raise LookupError(
                    f"paragraph {idx} does not contain: {anchor[:70]!r}\n  text: {whole[:200]!r}")
            end = m.end()
        else:
            end += len(anchor)

        for s0, s1, r, t in spans:
            if not (s0 < end <= s1):
                continue
            text = t.text or ""
            cut = end - s0
            head, tail = text[:cut], text[cut:]
            parent, at = r.getparent(), list(r.getparent()).index(r)
            t.text = head
            t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
            rpr = r.find(q("w:rPr"))
            rpr = copy.deepcopy(rpr) if rpr is not None else self._style_of(p)
            ## The insertion goes after the anchor's own run, or after that
            ## run's <w:ins> wrapper when the anchor is itself pending text, so
            ## the new sentence is a sibling of the existing revision rather
            ## than nested inside it.
            after = r if parent.tag != q("w:ins") else parent
            if tail:
                post = copy.deepcopy(r)
                for tt in post.findall(q("w:t")):
                    tt.text = tail
                    tt.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
                if parent.tag == q("w:ins"):
                    wrap = copy.deepcopy(parent)
                    for child in list(wrap):
                        wrap.remove(child)
                    wrap.set(q("w:id"), str(self.rid()))
                    wrap.append(post)
                    after.addnext(wrap)
                else:
                    r.addnext(post)
            after.addnext(self._ins([self._run(new, rpr)]))
            self.log.append(f"insert text in para {idx}: {note or new[:50]}")
            return
        raise LookupError(f"paragraph {idx}: could not split at the anchor")

    def replace_text(self, idx: int, old: str, new: str, note: str = "") -> None:
        """Replace one span of text inside a paragraph, leaving the rest alone.

        Whole-paragraph replacement would mark an entire paragraph as changed
        when one sentence moved, which makes a reviewer read every word to find
        the edit. This locates `old` in the paragraph's visible text, splits the
        runs so exactly that span is isolated, deletes it and inserts `new` in
        its place. Formatting is inherited from the run the span started in.
        """
        p = self.paragraphs()[idx]
        runs = [r for r in p.iter(q("w:r"))
                if r.getparent() is not None and r.getparent().tag != q("w:del")]

        # character offset -> (run, text element, offset within that element)
        spans, pos = [], 0
        for r in runs:
            for t in r.findall(q("w:t")):
                s = t.text or ""
                spans.append((pos, pos + len(s), r, t))
                pos += len(s)
        whole = "".join((t.text or "") for _, _, _, t in spans)

        norm = lambda s: re.sub(r"\s+", " ", s)
        start = whole.find(old)
        if start < 0:                       # tolerate whitespace differences
            m = re.search(re.escape(norm(old)).replace(r"\ ", r"\s+"), whole)
            if not m:
                raise LookupError(
                    f"paragraph {idx} does not contain: {old[:70]!r}\n  text: {whole[:200]!r}")
            start, end = m.start(), m.end()
        else:
            end = start + len(old)

        first_rpr = None
        targets = []
        for s0, s1, r, t in spans:
            if s1 <= start or s0 >= end:
                continue
            text = t.text or ""
            lo, hi = max(start - s0, 0), min(end - s0, len(text))
            head, mid, tail = text[:lo], text[lo:hi], text[hi:]
            rpr = r.find(q("w:rPr"))
            if first_rpr is None and rpr is not None:
                first_rpr = copy.deepcopy(rpr)
            parent, at = r.getparent(), list(r.getparent()).index(r)
            t.text = mid                     # this run now holds only the target
            if head:
                pre = copy.deepcopy(r)
                for tt in pre.findall(q("w:t")):
                    tt.text = head
                    tt.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
                parent.insert(at, pre); at += 1
            if tail:
                post = copy.deepcopy(r)
                for tt in post.findall(q("w:t")):
                    tt.text = tail
                    tt.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
                r.addnext(post)
            targets.append(r)

        last_wrapper = None
        for r in targets:
            parent, at = r.getparent(), list(r.getparent()).index(r)
            wrapper = self._del_wrap(r)
            parent.insert(at, wrapper)
            wrapper.append(r)
            last_wrapper = wrapper
        if new:
            # After the WRAPPER, not after the run: the run's parent is now the
            # <w:del>, so inserting relative to it would put the replacement
            # inside the deletion, where accepting the change would discard it.
            rpr = first_rpr if first_rpr is not None else self._style_of(p)
            last_wrapper.addnext(self._ins([self._run(new, rpr)]))
        self.log.append(f"replace text in para {idx}: {note or old[:50]}")

    # Successive insert_*_after() calls on one index each land immediately after
    # that paragraph, so they come out in reverse order of the calls. The
    # element-based variants below return the last element written, so a caller
    # can chain them and get document order. Index-based wrappers are kept for
    # single insertions.
    def paragraph_at(self, idx: int) -> etree._Element:
        return self.paragraphs()[idx]

    def insert_paragraphs_after_el(self, anchor: etree._Element, texts: list[str],
                                   model: etree._Element | None = None,
                                   bold_first: bool = False) -> etree._Element:
        model = model if model is not None else anchor
        rpr = self._style_of(model)
        ppr_src = model.find(q("w:pPr"))
        prev = anchor
        for n, text in enumerate(texts):
            p = etree.Element(q("w:p"))
            ppr = copy.deepcopy(ppr_src) if ppr_src is not None else etree.Element(q("w:pPr"))
            p.append(ppr)
            rpr_mark = ppr.find(q("w:rPr"))
            if rpr_mark is None:
                rpr_mark = etree.SubElement(ppr, q("w:rPr"))
            ins_mark = etree.Element(q("w:ins"))
            ins_mark.set(q("w:id"), str(self.rid()))
            ins_mark.set(q("w:author"), self.author)
            ins_mark.set(q("w:date"), self.date)
            ins_mark.set(q("w16du:dateUtc"), self.date_utc)
            rpr_mark.insert(0, ins_mark)
            run_rpr = copy.deepcopy(rpr) if rpr is not None else None
            if bold_first and n == 0 and run_rpr is not None and run_rpr.find(q("w:b")) is None:
                run_rpr.insert(0, etree.Element(q("w:b")))
            p.append(self._ins([self._run(text, run_rpr)]))
            prev.addnext(p)
            prev = p
        self.log.append(f"insert {len(texts)} paragraph(s)")
        return prev

    def insert_paragraphs_after(self, idx: int, texts: list[str],
                                style_from: int | None = None,
                                bold_first: bool = False) -> None:
        """Insert whole new paragraphs, each marked as an insertion."""
        paras = self.paragraphs()
        anchor = paras[idx]
        model = paras[style_from if style_from is not None else idx]
        rpr = self._style_of(model)
        ppr_src = model.find(q("w:pPr"))
        prev = anchor
        for n, text in enumerate(texts):
            p = etree.Element(q("w:p"))
            ppr = copy.deepcopy(ppr_src) if ppr_src is not None else etree.SubElement(p, q("w:pPr"))
            if ppr.getparent() is None:
                p.append(ppr)
            # the new paragraph mark is itself an insertion
            rpr_mark = ppr.find(q("w:rPr"))
            if rpr_mark is None:
                rpr_mark = etree.SubElement(ppr, q("w:rPr"))
            ins_mark = etree.Element(q("w:ins"))
            ins_mark.set(q("w:id"), str(self.rid()))
            ins_mark.set(q("w:author"), self.author)
            ins_mark.set(q("w:date"), self.date)
            ins_mark.set(q("w16du:dateUtc"), self.date_utc)
            rpr_mark.insert(0, ins_mark)
            run_rpr = copy.deepcopy(rpr) if rpr is not None else None
            if bold_first and n == 0 and run_rpr is not None:
                if run_rpr.find(q("w:b")) is None:
                    run_rpr.insert(0, etree.Element(q("w:b")))
            p.append(self._ins([self._run(text, run_rpr)]))
            prev.addnext(p)
            prev = p
        self.log.append(f"insert {len(texts)} paragraph(s) after {idx}")

    # ---- comments ---------------------------------------------------------
    def add_comment(self, idx: int, text: str) -> None:
        """Anchor a margin comment over the whole of paragraph `idx`."""
        cid = self._next_comment
        self._next_comment += 1
        p = self.paragraphs()[idx]

        start = etree.Element(q("w:commentRangeStart"))
        start.set(q("w:id"), str(cid))
        p.insert(len(p.findall(q("w:pPr"))), start)

        end = etree.SubElement(p, q("w:commentRangeEnd"))
        end.set(q("w:id"), str(cid))
        r = etree.SubElement(p, q("w:r"))
        rpr = etree.SubElement(r, q("w:rPr"))
        st = etree.SubElement(rpr, q("w:rStyle"))
        st.set(q("w:val"), "CommentReference")
        ref = etree.SubElement(r, q("w:commentReference"))
        ref.set(q("w:id"), str(cid))

        para_id = "%08X" % (0x10000000 + cid * 7919 % 0x0FFFFFFF)
        c = etree.SubElement(self.comments, q("w:comment"))
        c.set(q("w:id"), str(cid))
        c.set(q("w:author"), self.author)
        c.set(q("w:date"), self.date)
        c.set(q("w:initials"), self.initials)
        cp = etree.SubElement(c, q("w:p"))
        cp.set(q("w14:paraId"), para_id)
        cp.set(q("w14:textId"), "77777777")
        r1 = etree.SubElement(cp, q("w:r"))
        rpr1 = etree.SubElement(r1, q("w:rPr"))
        st1 = etree.SubElement(rpr1, q("w:rStyle"))
        st1.set(q("w:val"), "CommentReference")
        etree.SubElement(r1, q("w:annotationRef"))
        r2 = etree.SubElement(cp, q("w:r"))
        rpr2 = etree.SubElement(r2, q("w:rPr"))
        sz = etree.SubElement(rpr2, q("w:sz")); sz.set(q("w:val"), "20")
        szcs = etree.SubElement(rpr2, q("w:szCs")); szcs.set(q("w:val"), "20")
        t = etree.SubElement(r2, q("w:t"))
        t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
        t.text = text

        self._durable += 1
        dur = "%08X" % self._durable
        ce = etree.SubElement(self.cext, q("w15:commentEx"))
        ce.set(q("w15:paraId"), para_id)
        ce.set(q("w15:done"), "0")
        ci = etree.SubElement(self.cids, q("w16cid:commentId"))
        ci.set(q("w16cid:paraId"), para_id)
        ci.set(q("w16cid:durableId"), dur)
        cx = etree.SubElement(self.cex, q("w16cex:commentExtensible"))
        cx.set(q("w16cex:durableId"), dur)
        cx.set(q("w16cex:dateUtc"), self.date_utc)
        self.log.append(f"comment on para {idx}: {text[:50]}")

    # ---- tables -----------------------------------------------------------
    def insert_table_after(self, idx: int, rows: list[list[str]],
                           widths: list[int] | None = None) -> None:
        self.insert_table_after_el(self.paragraphs()[idx], rows, widths)

    def insert_table_after_el(self, anchor: etree._Element, rows: list[list[str]],
                              widths: list[int] | None = None,
                              size_half_pt: int | None = None) -> etree._Element:
        """A real Word table whose every row is marked as an inserted row.

        `size_half_pt` sets the cell font in half-points (18 = 9pt). A results
        table at body size wraps every numeric column onto two lines, which is
        unreadable; shrinking the cells is what a typesetter would do.
        """
        rpr = self._style_of(anchor)
        ncol = len(rows[0])
        widths = widths or [int(9000 / ncol)] * ncol

        tbl = etree.Element(q("w:tbl"))
        pr = etree.SubElement(tbl, q("w:tblPr"))
        st = etree.SubElement(pr, q("w:tblStyle")); st.set(q("w:val"), "TableGrid")
        wd = etree.SubElement(pr, q("w:tblW"))
        wd.set(q("w:w"), "0"); wd.set(q("w:type"), "auto")
        borders = etree.SubElement(pr, q("w:tblBorders"))
        for side in ("top", "left", "bottom", "right", "insideH", "insideV"):
            b = etree.SubElement(borders, q("w:" + side))
            b.set(q("w:val"), "single"); b.set(q("w:sz"), "4")
            b.set(q("w:space"), "0"); b.set(q("w:color"), "auto")
        grid = etree.SubElement(tbl, q("w:tblGrid"))
        for wv in widths:
            gc = etree.SubElement(grid, q("w:gridCol")); gc.set(q("w:w"), str(wv))

        for ri, row in enumerate(rows):
            tr = etree.SubElement(tbl, q("w:tr"))
            trpr = etree.SubElement(tr, q("w:trPr"))
            ins = etree.SubElement(trpr, q("w:ins"))
            ins.set(q("w:id"), str(self.rid()))
            ins.set(q("w:author"), self.author)
            ins.set(q("w:date"), self.date)
            ins.set(q("w16du:dateUtc"), self.date_utc)
            for ci_, cell in enumerate(row):
                tc = etree.SubElement(tr, q("w:tc"))
                tcpr = etree.SubElement(tc, q("w:tcPr"))
                tw = etree.SubElement(tcpr, q("w:tcW"))
                tw.set(q("w:w"), str(widths[ci_])); tw.set(q("w:type"), "dxa")
                p = etree.SubElement(tc, q("w:p"))
                ppr = etree.SubElement(p, q("w:pPr"))
                rmark = etree.SubElement(ppr, q("w:rPr"))
                im = etree.SubElement(rmark, q("w:ins"))
                im.set(q("w:id"), str(self.rid()))
                im.set(q("w:author"), self.author)
                im.set(q("w:date"), self.date)
                im.set(q("w16du:dateUtc"), self.date_utc)
                cell_rpr = copy.deepcopy(rpr) if rpr is not None else etree.Element(q("w:rPr"))
                if ri == 0 and cell_rpr.find(q("w:b")) is None:
                    cell_rpr.insert(0, etree.Element(q("w:b")))
                if size_half_pt is not None:
                    for tag in ("w:sz", "w:szCs"):
                        el = cell_rpr.find(q(tag))
                        if el is None:
                            el = etree.SubElement(cell_rpr, q(tag))
                        el.set(q("w:val"), str(size_half_pt))
                p.append(self._ins([self._run(cell, cell_rpr)]))
        anchor.addnext(tbl)
        self.log.append(f"insert {len(rows)}x{ncol} table")
        return tbl

    def insert_table_column(self, tbl: etree._Element, values: list[str],
                            width: int, widths: list[int] | None = None,
                            position: int | None = None) -> None:
        """Add one tracked column to an EXISTING table.

        `values` is one string per row including the header. `width` is the new
        column in twips. `widths` optionally restates every pre-existing column,
        which is usually necessary: a table already sized to the text block has
        no room for another column, and appending one without narrowing the rest
        pushes the table off the page. `position` defaults to the right-hand end.

        Word marks an inserted cell with <w:tcPr><w:cellIns/></w:tcPr>, the cell
        analogue of the <w:trPr><w:ins/></w:trPr> that marks an inserted row.
        The cell's content is additionally wrapped in <w:ins> so the text is
        struck through if the column is rejected in a viewer that does not
        implement cellIns.
        """
        rows = tbl.findall(q("w:tr"))
        if len(values) != len(rows):
            raise ValueError(
                f"insert_table_column: {len(values)} values for {len(rows)} rows. "
                "The header row needs a value too.")
        grid = tbl.find(q("w:tblGrid"))
        cols = grid.findall(q("w:gridCol"))
        at = len(cols) if position is None else position

        if widths is not None:
            if len(widths) != len(cols):
                raise ValueError(
                    f"insert_table_column: {len(widths)} widths for {len(cols)} "
                    "existing columns.")
            for gc, wv in zip(cols, widths):
                gc.set(q("w:w"), str(wv))

        gc = etree.Element(q("w:gridCol")); gc.set(q("w:w"), str(width))
        if at >= len(cols):
            grid.append(gc)
        else:
            cols[at].addprevious(gc)

        ## The style model is the first cell of the second row, so the new
        ## column inherits the table's body formatting rather than the
        ## document's default. Taking it from the header would make every cell
        ## bold; taking it from a chained insertion is how captions have
        ## previously inherited an image's centring.
        model_row = rows[1] if len(rows) > 1 else rows[0]
        model_tc = model_row.findall(q("w:tc"))[0]
        model_p = model_tc.find(q("w:p"))
        model_rpr = None
        if model_p is not None:
            ## Search DESCENDANTS, not direct children. Where the table is
            ## itself a pending insertion the cell's run sits inside a <w:ins>,
            ## so find() returns nothing and the fallback picks up the paragraph
            ## mark's own rPr, which carries the revision marker and no font.
            ## The new column then came out in the document default, a serif
            ## face beside seven columns of Arial.
            for r in model_p.iter(q("w:r")):
                if r.getparent() is not None and r.getparent().tag == q("w:del"):
                    continue
                if r.find(q("w:rPr")) is not None and len(r.find(q("w:rPr"))):
                    model_rpr = r.find(q("w:rPr"))
                    break
            if model_rpr is None and model_p.find(q("w:pPr")) is not None:
                model_rpr = model_p.find(q("w:pPr")).find(q("w:rPr"))
        model_ppr = model_p.find(q("w:pPr")) if model_p is not None else None

        for ri, (tr, text) in enumerate(zip(rows, values)):
            tcs = tr.findall(q("w:tc"))
            if widths is not None:
                ## Narrowing the existing columns is itself a revision. Recording
                ## the previous w:tcPr in a w:tcPrChange is how Word tracks it,
                ## and without it rejecting the column would remove the new cells
                ## and leave the table permanently resized: an untracked change
                ## surviving a rejection, which is the one thing a round-trip
                ## test exists to catch.
                for tc, wv in zip(tcs, widths):
                    tcpr = tc.find(q("w:tcPr"))
                    if tcpr is None:
                        continue
                    tw = tcpr.find(q("w:tcW"))
                    if tw is None or tw.get(q("w:w")) == str(wv):
                        continue
                    old = copy.deepcopy(tcpr)
                    for stale in old.findall(q("w:tcPrChange")):
                        old.remove(stale)
                    chg = etree.SubElement(tcpr, q("w:tcPrChange"))
                    chg.set(q("w:id"), str(self.rid()))
                    chg.set(q("w:author"), self.author)
                    chg.set(q("w:date"), self.date)
                    chg.append(old)
                    tw.set(q("w:w"), str(wv)); tw.set(q("w:type"), "dxa")

            tc = etree.Element(q("w:tc"))
            tcpr = etree.SubElement(tc, q("w:tcPr"))
            tw = etree.SubElement(tcpr, q("w:tcW"))
            tw.set(q("w:w"), str(width)); tw.set(q("w:type"), "dxa")
            ins = etree.SubElement(tcpr, q("w:cellIns"))
            ins.set(q("w:id"), str(self.rid()))
            ins.set(q("w:author"), self.author)
            ins.set(q("w:date"), self.date)

            p = etree.SubElement(tc, q("w:p"))
            if model_ppr is not None:
                ppr = copy.deepcopy(model_ppr)
                for stale in ppr.findall(q("w:rPr")):
                    ppr.remove(stale)
                p.append(ppr)
            rpr = copy.deepcopy(model_rpr) if model_rpr is not None else etree.Element(q("w:rPr"))
            if ri == 0 and rpr.find(q("w:b")) is None:
                rpr.insert(0, etree.Element(q("w:b")))
            p.append(self._ins([self._run(text, rpr)]))

            if at >= len(tcs):
                tr.append(tc)
            else:
                tcs[at].addprevious(tc)

        self.log.append(f"insert table column of {len(values)} cells at {at}")

    def delete_table_row(self, tr: etree._Element) -> None:
        """Mark one existing table row as deleted, as Word does: a <w:del> in the
        row's w:trPr plus every run of its cells wrapped in <w:del>."""
        trpr = tr.find(q("w:trPr"))
        if trpr is None:
            trpr = etree.Element(q("w:trPr"))
            tr.insert(0 if tr.find(q("w:tblPrEx")) is None else 1, trpr)
        mark = etree.SubElement(trpr, q("w:del"))
        mark.set(q("w:id"), str(self.rid()))
        mark.set(q("w:author"), self.author)
        mark.set(q("w:date"), self.date)
        for r in [r for r in tr.iter(q("w:r"))
                  if r.getparent() is not None and r.getparent().tag != q("w:del")]:
            parent = r.getparent()
            pos = list(parent).index(r)
            wrapper = self._del_wrap(r)
            parent.insert(pos, wrapper)
            wrapper.append(r)
        self.log.append("delete table row")

    # ---- images -----------------------------------------------------------
    def insert_image_after(self, idx: int, png: str, width_cm: float = 15.0) -> None:
        self.insert_image_after_el(self.paragraphs()[idx], png, width_cm)

    def insert_image_after_el(self, anchor: etree._Element, png: str,
                              width_cm: float = 15.0) -> etree._Element:
        from PIL import Image
        img = Path(png)
        data = img.read_bytes()
        with Image.open(img) as im:
            pw, ph = im.size
        cx = int(width_cm * 360000)
        cy = int(cx * ph / pw)

        n = len(self._media) + 1
        media_name = f"word/media/bayes_image{n}.png"
        self._media.append((media_name, data))
        rid = f"rIdBayes{n}"

        rels = etree.fromstring(self.parts["word/_rels/document.xml.rels"])
        rel = etree.SubElement(rels, "{http://schemas.openxmlformats.org/package/2006/relationships}Relationship")
        rel.set("Id", rid)
        rel.set("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image")
        rel.set("Target", f"media/bayes_image{n}.png")
        self.parts["word/_rels/document.xml.rels"] = etree.tostring(
            rels, xml_declaration=True, encoding="UTF-8", standalone=True)

        ct = etree.fromstring(self.parts["[Content_Types].xml"])
        cns = "{http://schemas.openxmlformats.org/package/2006/content-types}"
        if not any(d.get("Extension") == "png" for d in ct.findall(cns + "Default")):
            d = etree.SubElement(ct, cns + "Default")
            d.set("Extension", "png"); d.set("ContentType", "image/png")
            self.parts["[Content_Types].xml"] = etree.tostring(
                ct, xml_declaration=True, encoding="UTF-8", standalone=True)

        drawing_xml = f"""<w:drawing xmlns:w="{W}" xmlns:wp="{WP}" xmlns:a="{A}" xmlns:pic="{PIC}" xmlns:r="{R}">
  <wp:inline distT="0" distB="0" distL="0" distR="0">
    <wp:extent cx="{cx}" cy="{cy}"/>
    <wp:effectExtent l="0" t="0" r="0" b="0"/>
    <wp:docPr id="{900 + n}" name="Bayesian figure {n}"/>
    <wp:cNvGraphicFramePr><a:graphicFrameLocks noChangeAspect="1"/></wp:cNvGraphicFramePr>
    <a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">
      <pic:pic>
        <pic:nvPicPr><pic:cNvPr id="{900 + n}" name="figure{n}.png"/><pic:cNvPicPr/></pic:nvPicPr>
        <pic:blipFill><a:blip r:embed="{rid}"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill>
        <pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="{cx}" cy="{cy}"/></a:xfrm>
          <a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr>
      </pic:pic>
    </a:graphicData></a:graphic>
  </wp:inline>
</w:drawing>"""
        drawing = etree.fromstring(drawing_xml)

        p = etree.Element(q("w:p"))
        ppr = etree.SubElement(p, q("w:pPr"))
        jc = etree.SubElement(ppr, q("w:jc")); jc.set(q("w:val"), "center")
        rmark = etree.SubElement(ppr, q("w:rPr"))
        im = etree.SubElement(rmark, q("w:ins"))
        im.set(q("w:id"), str(self.rid())); im.set(q("w:author"), self.author)
        im.set(q("w:date"), self.date); im.set(q("w16du:dateUtc"), self.date_utc)
        r = etree.Element(q("w:r"))
        r.append(drawing)
        p.append(self._ins([r]))
        anchor.addnext(p)
        self.log.append(f"insert image {img.name}")
        return p

    # ---- output -----------------------------------------------------------
    def _serialise(self) -> dict[str, bytes]:
        out = dict(self.parts)
        ser = lambda e: etree.tostring(e, xml_declaration=True, encoding="UTF-8",
                                       standalone=True)
        out["word/document.xml"] = ser(self.doc)
        ## Comments parts are written only if the source had them or a comment
        ## was actually added. Writing four empty parts into a document whose
        ## [Content_Types].xml and relationships never declared them produces a
        ## file Word repairs on open, which is worse than not supporting
        ## comments there at all.
        if self._had_comments or len(self.comments):
            if not self._had_comments:
                raise RuntimeError(
                    "add_comment() was used on a document with no comments part. "
                    "Writing one requires new entries in [Content_Types].xml and "
                    "word/_rels/document.xml.rels, which this tool does not "
                    "create. Use tracked changes instead, or start from a "
                    "document that already carries a comment.")
            out["word/comments.xml"] = ser(self.comments)
            out["word/commentsExtensible.xml"] = ser(self.cex)
            out["word/commentsIds.xml"] = ser(self.cids)
            out["word/commentsExtended.xml"] = ser(self.cext)
        for name, data in self._media:
            out[name] = data
        return out

    def save(self, dest: str) -> None:
        parts = self._serialise()
        order = self.names + [n for n in parts if n not in self.names]
        with zipfile.ZipFile(dest, "w", zipfile.ZIP_DEFLATED) as z:
            for n in order:
                z.writestr(n, parts[n])


# ---- accept / reject ------------------------------------------------------

def _transform(src: str, dest: str, accept: bool, only=None) -> None:
    """Resolve every revision the way Word would, so the result can be rendered
    and compared. Accepting keeps insertions and drops deletions; rejecting does
    the reverse and turns w:delText back into w:t.

    `only`, if given, is a predicate on the revision marker element; revisions
    it rejects are left untouched. reject_new() uses it to undo one editing
    session while keeping every earlier revision, which is the check that a
    session's edits are fully reversible without disturbing anyone else's."""
    sel = (lambda els: els) if only is None else (lambda els: [e for e in els if only(e)])
    with zipfile.ZipFile(src) as z:
        parts = {n: z.read(n) for n in z.namelist()}
        order = z.namelist()
    doc = etree.fromstring(parts["word/document.xml"])

    ## Structural revisions FIRST, before the generic w:ins sweep below.
    ##
    ## An inserted row is marked <w:trPr><w:ins/></w:trPr> and an inserted cell
    ## <w:tcPr><w:cellIns/></w:tcPr>. Neither parent is w:rPr, so the sweep
    ## treated them as ordinary run insertions and stripped the marker. By the
    ## time the old code asked for //w:tr[w:trPr/w:ins] there was nothing left to
    ## match, and rejection silently kept every inserted row. A round trip that
    ## cannot remove what it inserted is not a check, so this ran ahead of it.
    if not accept:
        for m in sel(doc.xpath("//w:tr/w:trPr/w:ins", namespaces=NS)):
            tr = m.getparent().getparent()
            tr.getparent().remove(tr)
        ## An inserted column is a cell per row plus a w:gridCol. Both go. Any
        ## cell whose properties were changed to make room is restored from its
        ## w:tcPrChange, and the grid is then rebuilt from the restored cell
        ## widths, which is how Word derives it. Trimming the grid without
        ## restoring the widths would leave the table permanently narrowed.
        for tbl in doc.xpath("//w:tbl", namespaces=NS):
            removed = 0
            for tr in tbl.findall(q("w:tr")):
                cells = [m.getparent().getparent() for m in
                         sel(tr.xpath("./w:tc/w:tcPr/w:cellIns", namespaces=NS))]
                for tc in cells:
                    tr.remove(tc)
                removed = max(removed, len(cells))
            reverted = False
            for chg in sel(tbl.xpath(".//w:tcPr/w:tcPrChange", namespaces=NS)):
                tcpr = chg.getparent()
                old = chg.find(q("w:tcPr"))
                parent_tc = tcpr.getparent()
                if old is not None:
                    parent_tc.replace(tcpr, old)
                else:
                    tcpr.remove(chg)
                reverted = True
            grid = tbl.find(q("w:tblGrid"))
            if grid is not None and (removed or reverted):
                rows = tbl.findall(q("w:tr"))
                widths = []
                if rows:
                    for tc in rows[0].findall(q("w:tc")):
                        tw = tc.find(q("w:tcPr"))
                        tw = tw.find(q("w:tcW")) if tw is not None else None
                        widths.append(tw.get(q("w:w")) if tw is not None else None)
                cols = grid.findall(q("w:gridCol"))
                for gc in cols[len(widths):]:
                    grid.remove(gc)
                for gc, wv in zip(grid.findall(q("w:gridCol")), widths):
                    if wv is not None:
                        gc.set(q("w:w"), wv)
    else:
        ## Accepting a structural revision keeps the row, cell or width and
        ## drops only the marker, so the result is an ordinary table.
        for el in sel(doc.xpath("//w:trPr/w:ins | //w:tcPr/w:cellIns | //w:tcPr/w:tcPrChange",
                                namespaces=NS)):
            el.getparent().remove(el)
        ## A deleted row is marked <w:trPr><w:del/></w:trPr>. The generic sweep
        ## below would strip only the marker and leave an empty row behind, so
        ## accepting removes the row itself here.
        for m in sel(doc.xpath("//w:tr/w:trPr/w:del", namespaces=NS)):
            tr = m.getparent().getparent()
            tr.getparent().remove(tr)

    for el in sel(doc.xpath("//w:ins", namespaces=NS)):
        parent = el.getparent()
        if parent.tag == q("w:rPr"):            # inserted paragraph mark
            if accept:
                parent.remove(el)
            else:
                p = parent.getparent().getparent()   # rPr -> pPr -> p
                if p.getparent() is not None:
                    p.getparent().remove(p)
            continue
        if accept:
            i = list(parent).index(el)
            for child in list(el):
                parent.insert(i, child); i += 1
        parent.remove(el)

    for el in sel(doc.xpath("//w:del", namespaces=NS)):
        parent = el.getparent()
        if parent.tag == q("w:rPr"):
            parent.remove(el)
            continue
        if not accept:
            for t in el.iter(q("w:delText")):
                t.tag = q("w:t")
            i = list(parent).index(el)
            for child in list(el):
                parent.insert(i, child); i += 1
        parent.remove(el)

    if not accept:
        ## A wholly inserted table has lost every row above and is no longer a
        ## valid w:tbl, so it goes too.
        for tbl in doc.xpath("//w:tbl", namespaces=NS):
            if not tbl.findall(q("w:tr")):
                tbl.getparent().remove(tbl)

    parts["word/document.xml"] = etree.tostring(
        doc, xml_declaration=True, encoding="UTF-8", standalone=True)
    with zipfile.ZipFile(dest, "w", zipfile.ZIP_DEFLATED) as z:
        for n in order:
            z.writestr(n, parts[n])


def accept_all(src: str, dest: str) -> None:
    _transform(src, dest, accept=True)


def reject_all(src: str, dest: str) -> None:
    _transform(src, dest, accept=False)


def reject_new(src: str, dest: str, date_prefix: str) -> None:
    """Reject only the revisions whose w:date starts with `date_prefix`."""
    _transform(src, dest, accept=False,
               only=lambda el: (el.get(q("w:date")) or "").startswith(date_prefix))


def plain_text(path: str) -> str:
    """Visible text of a docx, for comparing a rejected copy with the original."""
    with zipfile.ZipFile(path) as z:
        doc = etree.fromstring(z.read("word/document.xml"))
    out = []
    for p in doc.iter(q("w:p")):
        out.append("".join(t.text or "" for t in p.iter(q("w:t"))))
    return "\n".join(out)
