"""Supplementary prior-grid table for the NBR revision, from tab_nbr.R output.

    python3 make_prior_grid_table.py PRIOR_GRID_CSV OUT_DOCX

Separates the four things a prior sensitivity analysis can show, which the
manuscript previously ran together: direction (P(ab > 0)), magnitude (ab and
its change from the primary fit), interval exclusion (95% CrI) and prediction
(95% new-study PI). Percentages withheld under the post hoc release rule are
labelled, never printed as estimates. Styling follows 'Table 2 (2).docx'
(A4 landscape, Calibri cells, Calibri 12 bold title, Arial 9 note).
"""
import csv
import sys

import docx
from docx.enum.section import WD_ORIENT
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Mm, Pt

src, out = sys.argv[1], sys.argv[2]
rows = list(csv.DictReader(open(src, newline="")))
assert len(rows) == 25, f"expected 5 primary + 20 grid rows, got {len(rows)}"

ORDER = ["dissoc_hal", "dissoc_del", "negsch_cps", "negsch_del", "depress_cps"]
LABEL = {"dissoc_hal": "Dissociation to hallucinations",
         "dissoc_del": "Dissociation to delusions",
         "negsch_cps": "Negative schemas to composite psychosis",
         "negsch_del": "Negative schemas to delusions",
         "depress_cps": "Depression to composite psychosis"}
PRIORS = ["primary", "alpha_sd=0.2", "alpha_sd=0.5", "tau_sd=0.1", "tau_sd=0.3"]
PRIOR_LABEL = {"primary": "Primary", "alpha_sd=0.2": "σα 0.20",
               "alpha_sd=0.5": "σα 0.50", "tau_sd=0.1": "στ 0.10", "tau_sd=0.3": "στ 0.30"}

f3 = lambda x: f"{float(x):.3f}"
def pfmt(x):
    x = float(x)
    return ">0.999" if x > 0.999 else f"{x:.3f}"
yes = lambda b: "Yes" if b else "No"
## A bound that rounds to zero at three decimals is printed to four, so its side of
## zero (the point of the Interval and Prediction columns) stays visible.
def fb(x):
    x = float(x)
    return f"{x:.4f}" if abs(x) < 0.0005 else f"{x:.3f}"
WORDS = {0: "No", 1: "One", 2: "Two", 3: "Three", 4: "Four"}

idx = {(r["pathway"], r["prior"]): r for r in rows}
assert set(idx) == {(p, q) for p in ORDER for q in PRIORS}

d = docx.Document()
sec = d.sections[0]
sec.orientation = WD_ORIENT.LANDSCAPE
sec.page_width, sec.page_height = Mm(297), Mm(210)
for side in ("left_margin", "right_margin", "top_margin", "bottom_margin"):
    setattr(sec, side, Mm(12))
d.styles["Normal"].font.name = "Times New Roman"

def run(p, text, font="Calibri", size=None, bold=False, sup=False):
    r = p.add_run(text)
    r.font.name = font
    r._element.rPr.rFonts.set(qn("w:eastAsia"), font)
    if size: r.font.size = Pt(size)
    r.bold = bold
    r.font.superscript = sup
    return r

title = d.add_paragraph()
run(title, "Table S23. Sensitivity of the indirect effect to the priors on the population-average "
           "correlations (σα) and between-cohort heterogeneity (στ)", size=12, bold=True)

HEAD1 = [("Pathway", 1), ("Prior", 1), ("Direction", 1), ("Magnitude", 2),
         ("Interval", 2), ("Prediction", 2), ("Percentage", 1)]
HEAD2 = ["", "", "P(ab > 0)", "ab", "Change from primary (%)", "95% CrI for ab",
         "Excludes zero", "95% new-study PI for ab", "Excludes zero",
         "% represented by indirect path [95% CrI]"]
ncol = len(HEAD2)
t = d.add_table(rows=2, cols=ncol)
t.alignment = WD_TABLE_ALIGNMENT.CENTER
t.style = d.styles["Table Grid"]

c = 0
for text, span in HEAD1:
    cell = t.cell(0, c)
    if span > 1: cell = cell.merge(t.cell(0, c + span - 1))
    if text in ("Pathway", "Prior"): cell = cell.merge(t.cell(1, c))
    cell.paragraphs[0].text = ""
    run(cell.paragraphs[0], text, size=9, bold=True)
    c += span
for j, text in enumerate(HEAD2):
    if text: run(t.cell(1, j).paragraphs[0], text, size=9, bold=True)

notes = []
for p in ORDER:
    for q in PRIORS:
        r = idx[(p, q)]
        prim = q == "primary"
        lo, hi = float(r["ab_crI_lo"]), float(r["ab_crI_hi"])
        plo, phi = float(r["ab_pi_lo"]), float(r["ab_pi_hi"])
        if r["pct_status"] == "released":
            pct = f"{float(r['pct']):.1f} [{float(r['pct_crI_lo']):.1f}, {float(r['pct_crI_hi']):.1f}]"
        else:
            notes.append(f"{LABEL[p]}, {PRIOR_LABEL[q]}: {r['pct_withheld_reason']}")
            pct = "withheld (rule)" + "*" * len(notes)
        cells = [LABEL[p] if prim else "", PRIOR_LABEL[q], pfmt(r["p_ab_gt0"]), f3(r["ab"]),
                 "(reference)" if prim else f"{float(r['ab_change_pct']):+.1f}",
                 f"[{fb(lo)}, {fb(hi)}]", yes(lo > 0 or hi < 0),
                 f"[{fb(plo)}, {fb(phi)}]", yes(plo > 0 or phi < 0), pct]
        tr = t.add_row().cells
        for j, v in enumerate(cells):
            run(tr[j].paragraphs[0], v, size=9, bold=prim and j == 0)

WIDTHS_MM = [48, 18, 19, 15, 23, 30, 17, 34, 17, 42]
t.autofit = False
## Word reads cell widths; LibreOffice reads the table grid. Set both.
for j, gc in enumerate(t._tbl.tblGrid.findall(qn("w:gridCol"))):
    gc.set(qn("w:w"), str(int(WIDTHS_MM[j] * 56.6929)))
for row in t.rows:
    for j, cell in enumerate(row.cells):
        cell.width = Mm(WIDTHS_MM[j])
## Repeat the two header rows on each printed page.
for i in (0, 1):
    trPr = t.rows[i]._tr.get_or_add_trPr()
    el = OxmlElement("w:tblHeader"); el.set(qn("w:val"), "true"); trPr.append(el)

alt = [r for r in rows if r["prior"] != "primary"]
minp = min(float(r["p_ab_gt0"]) for r in alt)
chg = [float(r["ab_change_pct"]) for r in alt]
n_cri0 = sum(float(r["ab_crI_lo"]) <= 0 for r in alt)
n_pix = sum(float(r["ab_pi_lo"]) > 0 for r in alt)
n_wh = sum(r["pct_status"] != "released" for r in alt)
## The note attributes every PI that excludes zero to the narrow heterogeneity prior; check it.
assert all(r["prior"] == "tau_sd=0.1" for r in alt if float(r["ab_pi_lo"]) > 0)

note = d.add_paragraph()
run(note, "Note. ", font="Arial", size=9, bold=True)
run(note, (
    "Each pathway was refitted with one prior changed at a time and every other setting held at "
    "its primary value (σα 0.35, στ 0.20). σα is the standard deviation of the normal prior on the population-average "
    "Fisher-z correlations; στ is the scale of the half-normal prior on the between-cohort standard "
    "deviations. ab is the standardised population-average indirect effect (posterior median). "
    "The columns separate four properties: direction, the posterior probability that ab is "
    "positive; magnitude, the posterior median of ab and its percentage change from the primary "
    "fit; interval, whether the 95% equal-tailed credible interval (CrI) for ab excludes zero; "
    "and prediction, whether the 95% new-study prediction interval (PI) for the latent true "
    "indirect effect in a new cohort excludes zero. "
    f"Across the 20 alternative-prior refits, the lowest posterior probability of a positive "
    f"indirect effect was approximately {minp:.3f} ({minp:.4f}), and ab changed by "
    f"{min(chg):.1f}% to +{max(chg):.1f}% relative to the primary fit. The CrI included zero in "
    f"{n_cri0} refits, and the PI excluded zero in {n_pix}, all under the narrower heterogeneity "
    "prior (στ 0.10), which shrinks the between-cohort spread and so narrows the PI by "
    "assumption rather than through data. Direction was therefore robust to the priors examined; "
    "magnitude, interval exclusion and prediction were less so. "
    "The percentage is 100 ab / (c′ + ab), a post hoc estimand, reported only where the fit passed "
    "every sampler diagnostic, P(total > 0.05) ≥ 0.95 and P(0 < percentage < 100) ≥ 0.95; "
    f"otherwise it is shown as withheld. {WORDS[n_wh]} alternative-prior percentages were withheld"
    + (": " + "; ".join(f"{'*' * (i + 1)} {n}" for i, n in enumerate(notes)) if notes else "")
    + ". Source: release run_0c04f897e2d1."), font="Arial", size=9)

d.save(out)
print(f"wrote {out}: min P {minp:.7f}; change {min(chg):.3f} to {max(chg):.3f}; "
      f"CrI incl 0 {n_cri0}; PI excl 0 {n_pix}; withheld {n_wh}")
