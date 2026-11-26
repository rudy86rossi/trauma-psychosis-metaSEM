"""Apply the statistical-review fixes on top of the current manuscript, Table 2 and Abstract.

    python3 fix_review.py SRC_DIR OUT_DIR

The manuscript carries RT's own Word edits made after track_main.py ran, so it
is edited where it stands rather than regenerated from the originals. Each fix
lies inside one of RT's pending 26 Nov 12:00 insertions, so its text is edited
in place, as Word does when an author edits their own pending insertion. No
other revision or comment is touched. The fixes in original text (the post hoc
wording and the Table S22 citation in the Table 2 footnote) are new tracked
changes, and the
co-author items are new comments, both dated 26 Nov 15:00. Every number is
checked against the release first.
"""
from __future__ import annotations

import csv
import math
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
from docx_track import TrackedDocx, q  # noqa: E402

NBR = HERE.parent.parent / "Neuroscience and Biobehavioral Reviews"
REL = Path.home() / "claude_code/trauma-psychosis-metaSEM/bayesian/results/run_0c04f897e2d1/tables"
SRC, OUT = Path(sys.argv[1]), Path(sys.argv[2])
OUT.mkdir(parents=True, exist_ok=True)
DATE = "2026-11-26T15:00:00Z"
OWN = "2026-11-26T12:00:00Z"

## ---- numbers ----
rd = lambda p: list(csv.DictReader(open(p, newline="")))
S = {r["fit_id"]: r for r in rd(NBR / "sensitivity_run_0c04f897e2d1.csv")}
G = [r for r in rd(NBR / "prior_grid_run_0c04f897e2d1.csv") if r["prior"] != "primary"]
ST = {(r["pathway"], r["fit_class"]): r for r in rd(REL / "proportion_mediated_stability.csv")}
HET = [float(r["median"]) for r in rd(REL / "heterogeneity.csv")
       if r["fit_class"] == "stan_primary" and r["moderator"] == "null" and r["quantity"] == "tau"]
cc = S["loso_stan|negsch_del|null|S025"]
nd, nc = ST[("negsch_del", "sens_loso_stan")], ST[("negsch_cps", "sens_loso_stan")]
ndg = ST[("negsch_del", "sens_prior_grid")]
assert f"{float(cc['p_pct_in_range']):.3f}" == "0.944" and cc["k"] == "4"
assert (nd["n_withheld"], nd["n_refits"], nc["n_withheld"], nc["n_refits"]) == ("5", "5", "5", "7")
assert (ndg["n_withheld"], ndg["n_refits"]) == ("2", "4")
assert round(math.erf(math.atanh(0.6) / 0.35 / math.sqrt(2)), 2) == 0.95 and round(1.96 * 0.20, 2) == 0.39
assert (f"{min(HET):.3f}", f"{max(HET):.3f}") == ("0.026", "0.292")
assert sum(float(r["ab_pi_lo"]) > 0 for r in G if r["prior"] == "tau_sd=0.1") == 3


def edit_own(t: TrackedDocx, needle: str, old: str, new: str) -> None:
    """Replace `old` by `new` where `old` lies wholly inside RT's own pending 12:00 insertions."""
    i = t.find_para(needle)
    p = t.paragraphs()[i]
    spans, pos = [], 0
    for r in p.iter(q("w:r")):
        if r.getparent() is not None and r.getparent().tag == q("w:del"):
            continue
        for tt in r.findall(q("w:t")):
            s = tt.text or ""
            spans.append((pos, pos + len(s), r, tt))
            pos += len(s)
    whole = "".join(tt.text or "" for *_, tt in spans)
    assert whole.count(old) == 1, f"{old[:60]!r}: {whole.count(old)} matches"
    a, b = whole.find(old), whole.find(old) + len(old)
    hit = [(s0, s1, r, tt) for s0, s1, r, tt in spans if s0 < b and s1 > a]
    for _, _, r, _ in hit:
        w = r.getparent()
        assert (w.tag == q("w:ins") and w.get(q("w:author")) == "Ricardo Twumasi"
                and w.get(q("w:date")) == OWN), f"{old[:60]!r} is not inside RT's own pending insertion"
    first = True
    for s0, s1, r, tt in hit:
        txt = tt.text
        lo, hi = max(a - s0, 0), min(b - s0, len(txt))
        tt.text = txt[:lo] + (new if first else "") + txt[hi:]
        tt.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
        first = False
    t.log.append(f"edit own insertion: {old[:50]}")


ABS = "Results: We included 117 studies"
def abstract(t: TrackedDocx) -> None:
    edit_own(t, ABS, "Standardised indirect effects were positive (0.053–0.175; probability",
             "Posterior median standardised indirect effects ranged from 0.053 to 0.175 (probability")
    edit_own(t, ABS, "so new studies may find little or none;",
             "so the true indirect association in a new setting could be small or absent;")


m = TrackedDocx(str(SRC / "Neuroscience&BiobehavioralMAIN_ 10.26.docx"), date=DATE, date_utc=DATE)
abstract(m)
edit_own(m, "Interpreting the Bayesian results. Priors were weakly informative",
         "for the between-study standard deviations; robustness",
         "for the between-study standard deviations (about 95% of the prior mass lies on correlations "
         "between -0.6 and 0.6, and on between-study standard deviations below 0.39 on the Fisher-z scale, "
         "respectively); robustness")
edit_own(m, "We additionally calculated the percentage of the pooled adversity",
         "case-control design (negative schemas to delusions)",
         "case-control design (negative schemas to delusions; exploratory, because only four samples remained)")
NS2 = "both indirect effects had 95% equal-tailed credible intervals"
edit_own(m, NS2, "left four samples for delusions; the indirect effect",
         "left four samples for delusions, below the five-sample minimum, so this check is exploratory; "
         "the indirect effect")
edit_own(m, NS2, "but its percentage was withheld under the stability rule.",
         "but its percentage was withheld under the stability rule (P(0 < percentage < 100)="
         f"{float(cc['p_pct_in_range']):.3f}).")
edit_own(m, NS2, "Both negative-schema percentages were sensitive to single-cohort deletion",
         f"Both negative-schema percentages were fragile: they were withheld in {nd['n_withheld']} of "
         f"{nd['n_refits']} single-cohort deletions for delusions and {nc['n_withheld']} of {nc['n_refits']} "
         "for composite psychosis")
edit_own(m, "Population-average indirect effects were positive across all",
         "all under the narrowest heterogeneity prior.",
         "all under the narrowest heterogeneity prior. The percentage for negative schemas to delusions was "
         "withheld under two of the four alternative priors.")

m.add_comment(m.find_para("There are important methodological limitations in both the review"),
              "RT (statistical review): 'substantial heterogeneity' here, and 'heterogeneity was substantial' "
              "in the Abstract, sit awkwardly with Section 3.3, which says heterogeneity was imprecisely "
              f"estimated and substantially prior-informed (posterior medians {min(HET):.3f} to {max(HET):.3f}; "
              "under the narrowest heterogeneity prior, three of the five prediction intervals exclude zero). "
              "Suggest 'possible substantial heterogeneity' here and 'heterogeneity was imprecisely "
              "estimated' in the Abstract.")
m.add_comment(m.find_para("The findings support the mediating roles of dissociation"),
              "RT (statistical review): 'support the mediating roles' reads as causal. Suggest 'are "
              "consistent with indirect associations via dissociation, negative schemas and depression', "
              "matching the Abstract's 'may partly account for'.")
m.add_comment(m.find_para("showing the largest average indirect effects. Evidence was stronger in non-clinical"),
              "RT (statistical review): two points. 'The largest average indirect effects': the intervals "
              "overlap and pathways were not formally compared, so suggest 'the largest point estimates of the "
              "indirect association among the synthesised pathways'. 'Evidence was stronger in non-clinical "
              "samples' (here and in the negative-schema paragraph) rests on the proportion of supportive "
              "analyses (for dissociation 84.4% against 48.8%), which depends on sample size and power (see "
              "Methodological considerations); the Bayesian sample-type moderator found no robust difference, "
              "so please say so alongside it.")
m.save(str(OUT / "Neuroscience&BiobehavioralMAIN_ 10.26.docx"))

a = TrackedDocx(str(SRC / "Abstract.docx"), date=DATE, date_utc=DATE)
abstract(a)
a.save(str(OUT / "Abstract.docx"))

t2 = TrackedDocx(str(SRC / "Table 2 (2).docx"), date=DATE, date_utc=DATE)
FN = "The percentage represented is the post hoc posterior median"
## "post hoc" here is the co-authors' original wording, so this is a tracked change.
fi = t2.find_para(FN)
t2.replace_text(fi, "the post hoc posterior median", "the posterior median")
t2.insert_text_after(fi, "of 100×ab/total association", ", a post hoc estimand")
t2.insert_text_after(fi, "Table S13, Fig S1, ", "Table S22 (single-cohort deletions), ")
## Table 2 (2).docx has no comments part, so its title comment sits in the manuscript.
m2 = TrackedDocx(str(OUT / "Neuroscience&BiobehavioralMAIN_ 10.26.docx"), date=DATE, date_utc=DATE)
m2.add_comment(m2.find_para("Meta-analyses of the five meta-analysed pathways are found in Table 2"),
               "RT (statistical review), on the Table 2 file: its title says 'mediated by', but the column heading and footnote "
               "describe the percentage as a decomposition of pooled associations, not a causal share. "
               "Suggest '... percentage of the association between childhood adversity and psychosis outcomes "
               "represented by the indirect path via psychological factors'.")
m2.save(str(OUT / "Neuroscience&BiobehavioralMAIN_ 10.26.docx"))
t2.save(str(OUT / "Table 2 (2).docx"))
for d in (m, a, t2, m2):
    print(d.src.name, "|", "; ".join(d.log))
