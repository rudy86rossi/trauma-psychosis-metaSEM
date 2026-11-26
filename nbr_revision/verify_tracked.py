"""Verify the tracked copies written by track_main.py before they replace the originals.

    python3 verify_tracked.py ORIGINAL_DIR TRACKED_DIR

For each document:
 1. Reject-new round trip: rejecting only today's revisions must reproduce the
    original, compared as a normalised token stream (runs merged by formatting
    and revision context, rsids ignored), with every earlier revision and
    comment in place. Run splitting is allowed; any change of text, formatting,
    structure, table grid or earlier revision is not.
 2. Earlier revisions and comments keep their ids, authors, dates and text.
 3. Every new revision is by Ricardo Twumasi, dated today, and no inserted run
    carries a copied rPrChange.
 4. New text: no em dashes, no US spellings, and every number is either in
    numbers_used.csv (formatted from the release) or on a short allowlist of
    fixed constants that the text defines (priors, thresholds, the example).
Exits 1 on any failure.
"""
from __future__ import annotations

import csv
import re
import sys
import tempfile
import zipfile
from pathlib import Path

from lxml import etree

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from docx_track import NS, q, reject_new  # noqa: E402

DATE = "2026-11-26"
AUTHOR = "Ricardo Twumasi"
M = "http://schemas.openxmlformats.org/officeDocument/2006/math"
orig_dir, trk_dir = Path(sys.argv[1]), Path(sys.argv[2])
DOCS = ["Neuroscience&BiobehavioralMAIN_ 10.26.docx", "Table 2 (2).docx", "Abstract.docx"]
fail: list[str] = []

IGN = re.compile(r"rsid|paraId|textId")
REV = {q("w:ins"), q("w:del")}


def attrs(el):
    return tuple(sorted((k.split("}")[-1], v) for k, v in el.attrib.items() if not IGN.search(k)))


def canon(el):
    if el is None:
        return ""
    s = etree.tostring(el).decode()
    s = re.sub(r'\s(w:)?rsid\w*="[^"]*"', "", s)
    return re.sub(r'\sxmlns:\w+="[^"]+"', "", s)


def tokens(doc, drop_comments: set[str]):
    out = []
    def ctx(el):
        c, a = [], el.getparent()
        while a is not None:
            if a.tag in REV:
                c.append(a.get(q("w:id")))
            a = a.getparent()
        return tuple(c)
    def walk(el):
        tag = el.tag
        if not isinstance(tag, str):
            return
        if tag in (q("w:commentRangeStart"), q("w:commentRangeEnd")) and el.get(q("w:id")) in drop_comments:
            return
        if tag in (q("w:r"), "{%s}r" % M):
            ref = el.find(q("w:commentReference"))
            if ref is not None and ref.get(q("w:id")) in drop_comments:
                return
            txt = []
            for x in el.iter():
                if x.tag in (q("w:t"), q("w:delText"), "{%s}t" % M):
                    txt.append(x.text or "")
                elif x.tag == q("w:tab"):
                    txt.append("\t")
                elif x.tag in (q("w:br"), q("w:cr")):
                    txt.append("\n")
                elif x.tag in (q("w:commentReference"), q("w:footnoteReference"), q("w:drawing"),
                               q("w:fldChar"), q("w:instrText"), q("w:sym")):
                    txt.append("<%s %s>" % (x.tag.split("}")[1], attrs(x)))
            rpr = el.find(q("w:rPr"))
            key = ("R", ctx(el), canon(rpr))
            if out and out[-1][0] == key:
                out[-1] = (key, out[-1][1] + "".join(txt))
            else:
                out.append((key, "".join(txt)))
            return
        if tag in (q("w:rPr"),) and el.getparent() is not None and el.getparent().tag in (q("w:r"),):
            return
        if tag in REV:              # wrappers are context, carried on the runs
            for c in el:
                walk(c)
            return
        out.append((("E", tag.split("}")[1], attrs(el)), ""))
        if tag in (q("w:pPr"), q("w:tcPr"), q("w:trPr"), q("w:tblPr"), q("w:sectPr")):
            out[-1] = (out[-1][0], canon(el))
            return
        for c in el:
            walk(c)
    walk(doc.find(q("w:body")))
    return out


def load(path):
    with zipfile.ZipFile(path) as z:
        doc = etree.fromstring(z.read("word/document.xml"))
        com = etree.fromstring(z.read("word/comments.xml")) if "word/comments.xml" in z.namelist() else None
    return doc, com


def revisions(doc):
    out = {}
    for el in doc.xpath("//w:ins|//w:del|//w:rPrChange|//w:pPrChange|//w:cellIns|//w:tcPrChange",
                        namespaces=NS):
        txt = "".join(x.text or "" for x in el.iter() if x.tag in (q("w:t"), q("w:delText"), "{%s}t" % M))
        out[el.get(q("w:id"))] = (el.tag.split("}")[1], el.get(q("w:author")), el.get(q("w:date")), txt)
    return out


used = {r["printed"] for r in csv.DictReader(open(trk_dir / "numbers_used.csv"))}
ALLOW = {"0.35", "0.20", "0.10", "0.05", "0.95", "2.5", "95", "100", "0", "5", "28", "34", "20", "3",
         "1", "10", "7", "6", "4", "117", "2", "S1", "S2", "S6", "S13", "S14", "S15", "S22", "S23"}
US = ["analyze", "analyzed", "behavior", "modeling", "modeled", "labeled", "center", "centered", "favor",
      "randomization", "generalization", "standardized", "summarized", "characterize", "color"]

for name in DOCS:
    o, t = orig_dir / name, trk_dir / name
    od, oc = load(o)
    td, tc = load(t)
    with tempfile.TemporaryDirectory() as tmp:
        rj = Path(tmp) / "rej.docx"
        reject_new(str(t), str(rj), DATE)
        rd, _ = load(rj)
    old_c = {c.get(q("w:id")) for c in oc.findall(q("w:comment"))} if oc is not None else set()
    new_c = ({c.get(q("w:id")) for c in tc.findall(q("w:comment"))} - old_c) if tc is not None else set()
    to, tr_ = tokens(od, set()), tokens(rd, new_c)
    if to != tr_:
        k = next((i for i, (a, b) in enumerate(zip(to, tr_)) if a != b), min(len(to), len(tr_)))
        fail.append(f"{name}: reject-new differs from original at token {k}:\n   orig {to[k] if k < len(to) else None}"
                    f"\n   rej  {tr_[k] if k < len(tr_) else None}")
    # earlier revisions and comments intact
    ro, rt = revisions(od), revisions(td)
    for i, v in ro.items():
        if i not in rt:
            fail.append(f"{name}: earlier revision {i} missing")
        elif rt[i][:3] != v[:3] or not rt[i][3].startswith(v[3][:0]) or (v[3] and v[3] not in rt[i][3]):
            fail.append(f"{name}: earlier revision {i} changed: {v} -> {rt[i]}")
    if oc is not None:
        for c in oc.findall(q("w:comment")):
            m = tc.find(f"{{{NS['w']}}}comment[@{{{NS['w']}}}id='{c.get(q('w:id'))}']")
            if m is None or canon(m) != canon(c):
                fail.append(f"{name}: comment {c.get(q('w:id'))} changed or missing")
    # new revisions
    new = {i: v for i, v in rt.items() if i not in ro}
    bad = {i: v for i, v in new.items() if v[1] != AUTHOR or not (v[2] or "").startswith(DATE)}
    if bad:
        fail.append(f"{name}: {len(bad)} new revisions not by {AUTHOR} on {DATE}: {list(bad.items())[:3]}")
    for ins in td.xpath("//w:ins", namespaces=NS):
        if (ins.get(q("w:date")) or "").startswith(DATE) and ins.xpath(".//w:rPrChange", namespaces=NS):
            fail.append(f"{name}: inserted run carries rPrChange (id {ins.get(q('w:id'))})")
    # new text checks
    ins_text = " ".join("".join(x.text or "" for x in el.iter() if x.tag in (q("w:t"), "{%s}t" % M))
                        for el in td.xpath("//w:ins", namespaces=NS) if (el.get(q("w:date")) or "").startswith(DATE))
    if tc is not None:
        ins_text += " " + " ".join("".join(c.itertext()) for c in tc.findall(q("w:comment"))
                                   if c.get(q("w:id")) in new_c)
    if "—" in ins_text:
        fail.append(f"{name}: em dash in new text")
    for w in US:
        if re.search(rf"\b{w}\b", ins_text, re.I):
            fail.append(f"{name}: US spelling {w!r} in new text")
    ## Signs kept, so a printed -0.035 is matched against the release's -0.035.
    nums = re.findall(r"S\d+|(?<![\w.])-?\d+(?:\.\d+)?", re.sub(r"\d{4}-\d\d-\d\d", " ", ins_text))
    ok = lambda n: (n in used or n in ALLOW or n.lstrip("-") in ALLOW or re.fullmatch(r"20\d\d", n)
                    or n == "0.999" and ">0.999" in used)
    unknown = sorted({n for n in nums if not ok(n)})
    for n in unknown:
        m = re.search(rf"(?<![\w.]){re.escape(n)}", ins_text)
        print(f"   context {n!r}: ...{ins_text[max(0, m.start() - 70):m.end() + 30]}...")
    print(f"{name}: tokens {len(to)}; earlier revisions {len(ro)}; new revisions {len(new)}; "
          f"new comments {len(new_c)}; numbers in new text {len(nums)}; unexplained {unknown}")
    if unknown:
        fail.append(f"{name}: numbers not traced to the release or allowlist: {unknown}")

if fail:
    print("\nFAIL"); [print(" -", f) for f in fail]; sys.exit(1)
print("\nALL CHECKS PASSED")
