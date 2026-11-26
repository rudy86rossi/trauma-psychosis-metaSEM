"""Check that write_responses.py changed only the authorised regions of the rebuttal.

    python3 -I verify_rebuttal.py ORIGINAL.docx NEW.docx

Every package part except word/document.xml must be byte-identical. In the
body, each paragraph must be unchanged, or be a plan paragraph whose runs
differ only between an identical prefix and suffix (the label and body runs),
or be one of the eleven primer paragraphs. No 'Plan (RT)', primer reference,
em dash or US spelling may remain in the responses. Exits 1 on failure.
"""
import re
import sys
import zipfile

from lxml import etree

W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
q = lambda t: "{%s}%s" % (W, t)
ser = etree.tostring
rt = lambda r: "".join(t.text or "" for t in r.iter(q("t")))
pt = lambda p: "".join(rt(r) for r in p.iter(q("r")))
a, b = zipfile.ZipFile(sys.argv[1]), zipfile.ZipFile(sys.argv[2])
fail = []
if a.namelist() != b.namelist():
    fail.append("package parts differ")
fail += [f"{n} changed" for n in a.namelist() if n != "word/document.xml" and a.read(n) != b.read(n)]
ca, cb = (list(etree.fromstring(z.read("word/document.xml")).find(q("body"))) for z in (a, b))
j, changed, removed = 0, 0, []
for e in ca:
    if j < len(cb) and ser(e) == ser(cb[j]):
        j += 1
        continue
    if j < len(cb) and "Plan (RT): " in pt(e) and "Response: " in pt(cb[j]):
        ea, eb = e.findall(q("r")), cb[j].findall(q("r"))
        i = 0
        while ser(ea[i]) == ser(eb[i]):
            i += 1
        k = 0
        while k < min(len(ea), len(eb)) - i and ser(ea[-1 - k]) == ser(eb[-1 - k]):
            k += 1
        if not ("".join(rt(r) for r in ea[i:len(ea) - k]).startswith("Plan (RT): ")
                and "".join(rt(r) for r in eb[i:len(eb) - k]).startswith("Response: ")):
            fail.append(f"changes outside the plan runs: {pt(e)[:60]!r}")
        if [ser(c) for c in e if c.tag != q("r")] != [ser(c) for c in cb[j] if c.tag != q("r")]:
            fail.append(f"non-run content changed: {pt(e)[:60]!r}")
        changed += 1
        j += 1
        continue
    removed.append(pt(e))
if j != len(cb):
    fail.append("new paragraphs added")
if changed != 10:
    fail.append(f"{changed} plan paragraphs changed, expected 10")
if len(removed) != 11 or removed[0] != "" or not removed[1].startswith("Statistical primer") \
        or not removed[-1].startswith("Reference: Dienes"):
    fail.append(f"removed paragraphs are not the primer: {[r[:40] for r in removed]}")
full = " ".join(pt(p) for p in cb)
resp = " ".join(pt(p) for p in cb if "Response: " in pt(p))
if "Plan (RT)" in full or "primer" in full.lower():
    fail.append("plan label or primer reference left")
if "—" in resp:
    fail.append("em dash in a response paragraph")
US = ["analyze", "analyzed", "behavior", "modeling", "modeled", "labeled", "center", "centered", "favor",
      "randomization", "generalization", "standardized", "summarized", "characterize", "color"]
fail += [f"US spelling {w!r}" for w in US if re.search(rf"\b{w}\b", resp, re.I)]
print(f"plan paragraphs changed {changed}; primer paragraphs removed {len(removed)}")
if fail:
    print("FAIL"); [print(" -", f) for f in fail]; sys.exit(1)
print("ALL CHECKS PASSED")
