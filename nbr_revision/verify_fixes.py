"""Verify fix_review.py output against the documents it started from.

    python3 verify_fixes.py BEFORE_DIR AFTER_DIR

For each document:
 1. Rejecting RT's 26 Nov 12:00 and 15:00 revisions gives the same token stream
    before and after, so nothing outside RT's own revisions changed.
 2. Every other revision (including RT's own Word edits at 13:3x) and every
    existing comment is unchanged; the 12:00 revisions keep their ids.
 3. New revisions and comments are by Ricardo Twumasi, dated 15:00.
 4. The accepted text differs only in the intended places, printed for review,
    with no em dash or US spelling.
Exits 1 on any failure.
"""
from __future__ import annotations

import difflib
import re
import sys
import tempfile
import zipfile
from pathlib import Path

from lxml import etree

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from docx_track import NS, _transform, accept_all, q  # noqa: E402

M = "http://schemas.openxmlformats.org/officeDocument/2006/math"
MINE = ("2026-11-26T12:00", "2026-11-26T15:00")
NEW = "2026-11-26T15:00"
DOCS = ["Neuroscience&BiobehavioralMAIN_ 10.26.docx", "Table 2 (2).docx", "Abstract.docx"]
IGN = re.compile(r"rsid|paraId|textId")
REV = {q("w:ins"), q("w:del")}
before, after = Path(sys.argv[1]), Path(sys.argv[2])
fail: list[str] = []


def canon(el):
    if el is None:
        return ""
    s = etree.tostring(el).decode()
    return re.sub(r'\sxmlns:\w+="[^"]+"', "", re.sub(r'\s(w:)?rsid\w*="[^"]*"', "", s))


def tokens(doc, drop: set[str]):
    out = []
    def walk(el):
        tag = el.tag
        if not isinstance(tag, str):
            return
        if tag in (q("w:commentRangeStart"), q("w:commentRangeEnd")) and el.get(q("w:id")) in drop:
            return
        if tag in (q("w:r"), "{%s}r" % M):
            ref = el.find(q("w:commentReference"))
            if ref is not None and ref.get(q("w:id")) in drop:
                return
            txt = "".join((x.text or "") for x in el.iter() if x.tag in (q("w:t"), q("w:delText"), "{%s}t" % M))
            key = ("R", canon(el.find(q("w:rPr"))))
            if out and out[-1][0] == key:
                out[-1] = (key, out[-1][1] + txt)
            else:
                out.append((key, txt))
            return
        if tag in REV:
            for c in el:
                walk(c)
            return
        out.append((("E", tag.split("}")[1],
                     tuple(sorted((k, v) for k, v in el.attrib.items() if not IGN.search(k)))), ""))
        if tag in (q("w:pPr"), q("w:tcPr"), q("w:trPr"), q("w:tblPr"), q("w:sectPr")):
            out[-1] = (out[-1][0], canon(el))
            return
        for c in el:
            walk(c)
    walk(doc.find(q("w:body")))
    return out


def load(path):
    with zipfile.ZipFile(path) as z:
        d = etree.fromstring(z.read("word/document.xml"))
        c = etree.fromstring(z.read("word/comments.xml")) if "word/comments.xml" in z.namelist() else None
    return d, c


def revs(doc):
    return {e.get(q("w:id")): (e.tag.split("}")[1], e.get(q("w:author")), e.get(q("w:date")),
                               "".join(x.text or "" for x in e.iter() if x.tag in (q("w:t"), q("w:delText"))))
            for e in doc.xpath("//w:ins|//w:del|//w:rPrChange|//w:pPrChange|//w:cellIns|//w:tcPrChange",
                               namespaces=NS)}


def text(path):
    d, _ = load(path)
    return ["".join(t.text or "" for t in p.iter(q("w:t"), "{%s}t" % M)) for p in d.iter(q("w:p"))]


for name in DOCS:
    b, a = before / name, after / name
    bd, bc = load(b)
    ad, ac = load(a)
    old_c = {c.get(q("w:id")) for c in bc.findall(q("w:comment"))} if bc is not None else set()
    new_c = ({c.get(q("w:id")) for c in ac.findall(q("w:comment"))} - old_c) if ac is not None else set()
    with tempfile.TemporaryDirectory() as tmp:
        rb, ra, xb, xa = (str(Path(tmp) / f) for f in ("rb.docx", "ra.docx", "xb.docx", "xa.docx"))
        mine = lambda e: (e.get(q("w:date")) or "").startswith(MINE)
        _transform(str(b), rb, accept=False, only=mine)
        _transform(str(a), ra, accept=False, only=mine)
        tb, ta = tokens(load(rb)[0], set()), tokens(load(ra)[0], new_c)
        if tb != ta:
            k = next((i for i, (x, y) in enumerate(zip(tb, ta)) if x != y), min(len(tb), len(ta)))
            fail.append(f"{name}: text outside RT's revisions changed at token {k}: "
                        f"{tb[k] if k < len(tb) else None} -> {ta[k] if k < len(ta) else None}")
        accept_all(str(b), xb)
        accept_all(str(a), xa)
        pb, pa = text(xb), text(xa)
    if len(pb) != len(pa):
        fail.append(f"{name}: paragraph count changed {len(pb)} -> {len(pa)}")
    ## Accepted paragraphs are compared by position; only the inserted fragments
    ## are checked for style, so existing co-author text is not held against us.
    diff, frags = [], []
    for x, y in zip(pb, pa):
        if x != y:
            diff.append(y)
            sm_ = difflib.SequenceMatcher(None, x.split(" "), y.split(" "), autojunk=False)
            frags += [" ".join(y.split(" ")[j1:j2]) for op, _, _, j1, j2 in sm_.get_opcodes()
                      if op in ("insert", "replace")]
    rb_, ra_ = revs(bd), revs(ad)
    for i, v in rb_.items():
        if i not in ra_:
            fail.append(f"{name}: revision {i} missing")
        elif not (v[2] or "").startswith(MINE[0]) and ra_[i] != v:
            fail.append(f"{name}: revision {i} by {v[1]} at {v[2]} changed")
        elif ra_[i][:3] != v[:3]:
            fail.append(f"{name}: revision {i} metadata changed")
    for i, v in ra_.items():
        if i not in rb_ and (v[1] != "Ricardo Twumasi" or not (v[2] or "").startswith(NEW)):
            fail.append(f"{name}: new revision {i} not RT at 15:00: {v}")
    if bc is not None:
        for c in bc.findall(q("w:comment")):
            m = ac.find(f"{{{NS['w']}}}comment[@{{{NS['w']}}}id='{c.get(q('w:id'))}']")
            if m is None or canon(m) != canon(c):
                fail.append(f"{name}: comment {c.get(q('w:id'))} changed or missing")
    if ac is not None:
        for c in ac.findall(q("w:comment")):
            if c.get(q("w:id")) in new_c and not (c.get(q("w:author")) == "Ricardo Twumasi"
                                                   and c.get(q("w:date")).startswith(NEW)):
                fail.append(f"{name}: new comment {c.get(q('w:id'))} not RT at 15:00")
    added = " ".join(frags)
    added += " " + " ".join("".join(c.itertext()) for c in (ac.findall(q("w:comment")) if ac is not None else [])
                            if c.get(q("w:id")) in new_c)
    if "—" in added:
        fail.append(f"{name}: em dash in new text")
    for w in ["analyze", "behavior", "modeling", "labeled", "favor", "standardized", "color"]:
        if re.search(rf"\b{w}\b", added, re.I):
            fail.append(f"{name}: US spelling {w!r}")
    print(f"\n== {name}: new revisions {len(set(ra_) - set(rb_))}, new comments {len(new_c)}, "
          f"changed paragraphs (accepted) {len(diff)}")
    for f in frags:
        print("  +", f)

if fail:
    print("\nFAIL"); [print(" -", f) for f in fail]; sys.exit(1)
print("\nALL CHECKS PASSED")
