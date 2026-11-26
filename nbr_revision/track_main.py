"""Tracked changes for the NBR revision: manuscript, Table 2 and Abstract.

    python3 track_main.py OUT_DIR

Writes tracked copies of the three documents into OUT_DIR. Nothing is written
in place by this script; verify_tracked.py checks the copies, and only then are
they copied over the originals.

Every edit is anchored on distinctive text, never on a paragraph index, and
fails loudly if the anchor is missing or ambiguous. Every number in the new
text is formatted from the release extracts (tab_nbr.R output and the release
tables), never typed, and is logged to OUT_DIR/numbers_used.csv for the audit.
Author Ricardo Twumasi (RT). British English, no em dashes.
"""
from __future__ import annotations

import csv
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
from docx_track import TrackedDocx, q  # noqa: E402

NBR = HERE.parent.parent / "Neuroscience and Biobehavioral Reviews"
RUN = "run_0c04f897e2d1"
REL = Path.home() / "claude_code/trauma-psychosis-metaSEM/bayesian/results" / RUN / "tables"
DATE, DATE_UTC = "2026-11-26T12:00:00Z", "2026-11-26T12:00:00Z"
OUT = Path(sys.argv[1])
OUT.mkdir(parents=True, exist_ok=True)

# ---- numbers -----------------------------------------------------------------
rd = lambda p: list(csv.DictReader(open(p, newline="")))
PRIM = {r["pathway"]: r for r in rd(NBR / f"primary_{RUN}.csv")}
SENS = {r["fit_id"]: r for r in rd(NBR / f"sensitivity_{RUN}.csv")}
GRID = [r for r in rd(NBR / f"prior_grid_{RUN}.csv") if r["prior"] != "primary"]
HET = [r for r in rd(REL / "heterogeneity.csv")
       if r["fit_class"] == "stan_primary" and r["moderator"] == "null" and r["quantity"] == "tau"]
ELIG = rd(REL / "synthesis_eligibility.csv")
assert sorted(PRIM) == ["depress_cps", "dissoc_del", "dissoc_hal", "negsch_cps", "negsch_del"]

USED: list[tuple[str, str, str]] = []
def n3(x, what=""):
    s = f"{float(x):.3f}"; USED.append((what, str(x), s)); return s
def n1(x, what=""):
    s = f"{float(x):.1f}"; USED.append((what, str(x), s)); return s
def pf(x, what=""):
    x = float(x); s = ">0.999" if x > 0.999 else f"{x:.3f}"; USED.append((what, str(x), s)); return s
def ci(lo, hi, what=""):
    return f"[{n3(lo, what + ' lo')}, {n3(hi, what + ' hi')}]"
def ab(pw):
    r = PRIM[pw]; return f"ab={n3(r['ab'], pw + ' ab')} {ci(r['ab_crI_lo'], r['ab_crI_hi'], pw + ' ab CrI')}"
def pct(pw):
    r = PRIM[pw]; assert r["pct_status"] == "released", pw
    return f"{n1(r['pct'], pw + ' pct')}% (95% CrI {n1(r['pct_crI_lo'], pw + ' pct lo')} to {n1(r['pct_crI_hi'], pw + ' pct hi')})"

P = PRIM
ab_lo = min(P.values(), key=lambda r: float(r["ab"])); ab_hi = max(P.values(), key=lambda r: float(r["ab"]))
tot_lo = min(P.values(), key=lambda r: float(r["total"])); tot_hi = max(P.values(), key=lambda r: float(r["total"]))
pc_lo = min(P.values(), key=lambda r: float(r["pct"])); pc_hi = max(P.values(), key=lambda r: float(r["pct"]))
p_lo = min(P.values(), key=lambda r: float(r["p_ab_gt0"]))
assert all(float(r["total_crI_lo"]) > 0 for r in P.values())
assert all(float(r["ab_pi_lo"]) < 0 < float(r["ab_pi_hi"]) for r in P.values())
assert ab_lo["pathway"] == "negsch_cps" and ab_hi["pathway"] == "dissoc_hal"
assert float(P["negsch_cps"]["ab_hdi_lo"]) < 0 < float(P["negsch_cps"]["ab_crI_lo"])
het_lo = min(HET, key=lambda r: float(r["median"])); het_hi = max(HET, key=lambda r: float(r["median"]))
assert (het_hi["pathway"], het_hi["coordinate"]) == ("negsch_cps", "r_XM")
k_synth = sum(int(r["k_independent"]) for r in ELIG if r["eligible"] == "TRUE")
assert k_synth == 34 and [r["pathway"] for r in ELIG if r["eligible"] != "TRUE"] == ["dissoc_cps"]
N_DISTINCT = 28   # distinct sample identifiers across the five synthesised pathways (lock; Checkpoint 2)
app = SENS["loso_stan|negsch_del|null|S025"]; assert app["pct_status"] == "withheld"
dh, dd = SENS["direct_r|dissoc_hal|null"], SENS["direct_r|dissoc_del|null"]
minp = min(float(r["p_ab_gt0"]) for r in GRID)
chg = [float(r["ab_change_pct"]) for r in GRID]
cri0 = [r for r in GRID if r["crI_includes_zero"] == "TRUE"]
pix = [r for r in GRID if r["pi_includes_zero"] == "FALSE"]
assert len(GRID) == 20 and {r["pathway"] for r in cri0} == {"negsch_cps"} and len(cri0) == 3
assert len(pix) == 3 and {r["prior"] for r in pix} == {"tau_sd=0.1"}
USED += [("grid min P", str(minp), "0.966"), ("grid change min", str(min(chg)), f"{min(chg):.1f}"),
         ("grid change max", str(max(chg)), f"{max(chg):.1f}")]
## Single-cohort deletion: pathways whose percentage was withheld in any deletion
## (release table proportion_mediated_stability.csv).
STAB = [r for r in rd(REL / "proportion_mediated_stability.csv") if r["fit_class"] == "sens_loso_stan"]
fragile = sorted(r["pathway"] for r in STAB if int(r["n_withheld"]) > 0)
assert fragile == ["dissoc_hal", "negsch_cps", "negsch_del"], fragile
dh_loso = next(r for r in STAB if r["pathway"] == "dissoc_hal")
assert (dh_loso["n_withheld"], dh_loso["n_refits"]) == ("5", "10")

# ---- shared abstract edits ---------------------------------------------------
def abstract_edits(t: TrackedDocx, comment: bool = True) -> None:
    ## Kept short: the original Abstract was 246 words against an apparent
    ## 250-word limit, so the total association stays in the Results and Table 2.
    i = t.find_para("Results: We included 117 studies")
    t.replace_text(i, "Median percentages mediated were", "Descriptive median percentages mediated were")
    t.replace_text(i, "Six pathways (k=38) covering dissociation, negative schemas and depression were synthesised.",
                   f"Five pathways from {N_DISTINCT} samples, covering dissociation, negative schemas and depression, "
                   "met the five-sample minimum for Bayesian synthesis.")
    t.replace_text(i, "Population-average indirect effects were positive (0.053–0.162), with probabilities of a "
                      "positive effect of 0.976 to >0.999, representing 20.8%–55.5% of the total association.",
                   f"Standardised indirect effects were positive ({n3(ab_lo['ab'], 'abs ab lo')}–"
                   f"{n3(ab_hi['ab'], 'abs ab hi')}; probability of a positive effect "
                   f"{pf(p_lo['p_ab_gt0'], 'abs P lo')} to >0.999).")
    t.replace_text(i, "all 95% new-study prediction intervals included zero, heterogeneity",
                   "all 95% new-study prediction intervals included zero, so new studies may find little or none; "
                   "heterogeneity")
    i = t.find_para("Conclusions: Dissociation, psychosocial stressors")
    t.replace_text(i, "Dissociation, psychosocial stressors, negative schemas and depression may partly account",
                   "Dissociation, negative schemas and depression, the families with sufficient data for synthesis, "
                   "may partly account")
    if not comment:     # Abstract.docx has no comments part; the note lives in the manuscript
        return
    i = t.find_para("Results: We included 117 studies")
    t.add_comment(i, "RT: As agreed, the model-based percentage has left the Abstract, which now reports the "
                     "standardised indirect effect (ab); the total association is in the Results and Table 2. I kept "
                     "the descriptive median percentages from individual studies, labelled as descriptive so they are "
                     "not read as model estimates; delete them too if you would rather the Abstract carried no "
                     "percentages. The Abstract is now slightly longer than before; please check it against the journal's word "
                     "limit, which I could not confirm.")

# ---- manuscript --------------------------------------------------------------
def manuscript() -> TrackedDocx:
    t = TrackedDocx(str(NBR / "Neuroscience&BiobehavioralMAIN_ 10.26.docx"), date=DATE, date_utc=DATE_UTC)
    abstract_edits(t)
    ## RT's own placeholder (w:ins id 2, comment 1) is superseded by the new
    ## Results sentences; delete its text as a tracked change and keep both.
    i = t.find_para("Results: We included 117 studies")
    t.replace_text(i, "Standardised indirect effect…", "", note="delete RT placeholder")

    # Methods: eligibility rule and synthesised pathways
    i = t.find_para("Bayesian random-effects meta-analytic structural equation models (Cheung, 2022) were fitted")
    t.replace_text(i, "were fitted for six prespecified pathways with complete", "were fitted for pathways with complete")
    t.replace_text(i, "Five pathways included five to ten samples; one included four and was retained as a fragile synthesis.",
                   "In line with the minimum of five studies specified in the PROSPERO pre-registration, a pathway was "
                   "synthesised only where at least five distinct samples contributed. Five pathways met this criterion, "
                   f"with five to ten samples each ({k_synth} sample-by-pathway contributions from {N_DISTINCT} distinct "
                   "samples, some samples contributing to more than one pathway). Dissociation to composite psychosis "
                   "(four samples) did not, and post-traumatic stress symptoms, anxiety, attachment and psychosocial "
                   "stressors each had fewer than five eligible samples, so these were described in the descriptive "
                   "synthesis only.")

    # Methods: new interpretation paragraph after the moderator paragraph
    i = t.find_para("Models accounted for the dependence between correlations estimated from the same participants")
    t.insert_paragraphs_after_el(t.paragraph_at(i), [
        "Interpreting the Bayesian results. Priors were weakly informative and centred on zero: normal with "
        "standard deviation 0.35 for the population-average Fisher-z correlations and half-normal with scale 0.20 "
        "for the between-study standard deviations; robustness to these choices was examined with alternative "
        "priors (Table S23 in SM). Each model combines the prior with the data to give a posterior distribution, "
        "summarised by its median. A 95% credible interval (CrI) contains the population-average value with 95% "
        "probability given the data and model, whereas a confidence interval describes how often the procedure "
        "would capture the true value across repeated studies. We report two credible intervals: the equal-tailed "
        "interval, which excludes 2.5% of the posterior from each tail, and the highest-density interval (HDI), the "
        "narrowest interval containing 95% of it. They usually agree, but when the posterior lies close to zero one "
        "can exclude zero while the other includes it; this reflects how each interval is constructed, not "
        "conflicting analyses. The posterior probability P(ab > 0) is the probability, given the data and model, "
        "that the population-average indirect association is positive; it is not one minus a p value and says "
        "nothing about the size of the effect. The 95% new-study prediction interval describes the latent true "
        "indirect effect expected in a new sample from the same population of studies. Because it includes "
        "between-study heterogeneity it is wider than the credible interval, and it can include zero even when the "
        "average indirect association is very probably positive. In each model, a is the standardised association "
        "between CA and the mediator, b is the standardised association between the mediator and psychosis holding "
        "CA constant, and c′ is the standardised association between CA and psychosis holding the mediator "
        "constant. The indirect effect ab is the product of a and b, and the total association, c′ + ab, equals the "
        "CA–psychosis correlation. Because all are standardised, ab = 0.10 means that a one standard deviation "
        "difference in CA is associated with a 0.10 standard deviation difference in psychosis through the mediator "
        "path. These are associations estimated from largely cross-sectional data and do not identify causal "
        "mediation."], model=t.paragraph_at(i))

    # Methods: percentage rule is post hoc; added sensitivity analyses
    i = t.find_para("We additionally calculated the percentage of the pooled adversity")
    t.replace_text(i, "it was reported only when prespecified stability criteria were met.",
                   "it was reported only when stability criteria, adopted post hoc, were met: the fit passed every "
                   "sampler diagnostic, the posterior probability that the total association exceeded 0.05 was at "
                   "least 0.95, and the posterior probability that the percentage lay between 0 and 100 was at least 0.95.")
    t.insert_text_after(i, "leave-one-cohort-out analyses were examined.",
                        " Two further sensitivity analyses removed the only sample recruited by case-control design "
                        "(negative schemas to delusions) and, as specified in the protocol, restricted each pathway to "
                        "correlations taken directly from published correlation matrices rather than converted from "
                        "other statistics, where at least five samples remained.")

    # Results
    i = t.find_para("117 studies met our inclusion criteria and were included in this review")
    t.replace_text(i, "(ten samples for hallucinations, seven for delusions, four for composite psychosis)",
                   "(ten samples for hallucinations and seven for delusions)")
    i = t.find_para("Meta-analyses of the six meta-analysed pathways are found in Table 2")
    t.replace_text(i, "six meta-analysed pathways", "five meta-analysed pathways")

    i = t.find_para("In the Bayesian synthesis (Table 2), all three analyses")
    t.replace_text(i, "all three analyses indirect effects’ confident intervals did not cross zero, with the largest "
                      "effects being for delusions (k=7) and hallucinations (k=10; ab=0.160 [0.083, 0.239]).",
                   "the indirect effects of both dissociation pathways had 95% credible intervals that did not cross "
                   f"zero: hallucinations (k=10; {ab('dissoc_hal')}) and delusions (k=7; {ab('dissoc_del')}).")
    t.replace_text(i, "was 30.8% (95% CrI 1.7 to 85.4) for composite psychosis, 44.4% (95% CrI 20.4 to 83.9) for "
                      "delusions and 55.5% (95% CrI 27.7 to 99.7) for hallucinations.",
                   f"was {pct('dissoc_del')} for delusions and {pct('dissoc_hal')} for hallucinations. The upper "
                   "limit above 100% for hallucinations arises because the direct path c′ could be negative within "
                   "its credible interval, and this percentage was withheld under the stability rule in "
                   f"{dh_loso['n_withheld']} of {dh_loso['n_refits']} single-cohort deletions (Table S22 in SM). "
                   "Dissociation to composite psychosis had only four contributing samples and was not synthesised.")

    i = t.find_para("In the Bayesian synthesis (Table 2), all two analyses")
    t.replace_text(i, "all two analyses reported indirect effects which confident intervals did not cross zero, both "
                      "with composite psychosis (k=7; ab=0.162 [0.077, 0.248]) and delusions (k= 5; ab=0.065 [0.011, 0.130]).",
                   "both indirect effects had 95% equal-tailed credible intervals that did not cross zero, for "
                   f"composite psychosis (k=7; {ab('negsch_cps')}) and delusions (k=5; {ab('negsch_del')}). For "
                   "composite psychosis the lower limit was close to zero and the 95% highest-density interval "
                   f"included it ({ci(P['negsch_cps']['ab_hdi_lo'], P['negsch_cps']['ab_hdi_hi'], 'negsch_cps HDI')}), "
                   "so this estimate is less secure.")
    t.insert_text_after(i, "for delusions.",
                        " Removing the only case-control sample left four samples for delusions; the indirect effect "
                        f"was similar (ab={n3(app['ab'], 'appiah ab')} {ci(app['ab_crI_lo'], app['ab_crI_hi'], 'appiah CrI')}; "
                        f"P(ab > 0)={pf(app['p_ab_gt0'], 'appiah P')}), but its percentage was withheld under the "
                        "stability rule. Both negative-schema percentages were sensitive to single-cohort deletion "
                        "(Table S22 in SM).")

    i = t.find_para("In the Bayesian synthesis (Table 2), the only meta-analyses examined composite psychosis")
    t.replace_text(i, "whose confidence intervals did not cross zero", "whose credible intervals did not cross zero")

    # Summary paragraph
    i = t.find_para("Population-average indirect effects were positive across all six pathways")
    t.replace_text(i, "positive across all six pathways, ranging from 0.053 for negative schemas to composite "
                      "psychosis, to 0.162 for dissociation to delusions",
                   f"positive across all five pathways, ranging from {n3(ab_lo['ab'], 'sum ab lo')} for negative "
                   f"schemas to composite psychosis, to {n3(ab_hi['ab'], 'sum ab hi')} for dissociation to hallucinations")
    t.replace_text(i, "The estimated indirect pathways represented 20.8–55.5% of the pooled adversity–psychosis association.",
                   f"Standardised total associations ranged from {n3(tot_lo['total'], 'sum tot lo')} to "
                   f"{n3(tot_hi['total'], 'sum tot hi')}, and the estimated indirect pathways represented "
                   f"{n1(pc_lo['pct'], 'sum pct lo')}–{n1(pc_hi['pct'], 'sum pct hi')}% of them. Each percentage is a "
                   "ratio within one pathway, whose mediator, outcome and samples differ from those of the others, so "
                   "percentages are not directly comparable across pathways; the standardised indirect effects in "
                   "Table 2 are on a common scale, although they too compare associations rather than mechanisms.")
    t.replace_text(i, "ranging from 0.026 to 0.313 on", f"ranging from {n3(het_lo['median'], 'het lo')} to "
                                                       f"{n3(het_hi['median'], 'het hi')} on")
    t.replace_text(i, "the mediator–psychosis correlation in the dissociation-to-hallucinations pathway",
                   "the CA–mediator correlation in the negative-schemas-to-composite-psychosis pathway")
    replace_math(t, i, "τ=0.313", f"τ={n3(het_hi['median'], 'het hi math')}")
    t.replace_text(i, "95% CrI [0.211, 0.485]", f"95% CrI {ci(het_hi['q_lo'], het_hi['q_hi'], 'het hi CrI')}")
    t.replace_text(i, "only four to ten cohorts", "only five to ten cohorts")
    t.replace_text(i, "Fig S11 and Table S13 SM", "Fig S1 and Table S13 SM")
    t.replace_text(i, "for dissociation to composite psychosis and both negative-schema pathways were sensitive to "
                      "single-cohort deletion (table S17, S18).",
                   "for dissociation to hallucinations and both negative-schema pathways were sensitive to "
                   "single-cohort deletion (Table S22). Across 20 refits under alternative priors (Table S23), the "
                   "direction of every indirect effect was robust (lowest posterior probability of a positive effect "
                   f"approximately {minp:.3f}), but its magnitude changed by {min(chg):.1f}% to +{max(chg):.1f}%, the "
                   "credible interval included zero in three refits (all negative schemas to composite psychosis) and "
                   "the prediction interval excluded zero in three, all under the narrowest heterogeneity prior. "
                   "Restricting the dissociation pathways to correlations taken directly from correlation matrices, as "
                   "specified in the protocol, gave similar indirect effects (hallucinations, k="
                   f"{dh['k']}: ab={n3(dh['ab'], 'direct hal ab')} {ci(dh['ab_crI_lo'], dh['ab_crI_hi'], 'direct hal CrI')}; "
                   f"delusions, k={dd['k']}: ab={n3(dd['ab'], 'direct del ab')} "
                   f"{ci(dd['ab_crI_lo'], dd['ab_crI_hi'], 'direct del CrI')}); the negative-schema pathways had too "
                   "few such samples to refit, and every depression correlation was taken directly.")
    t.replace_text(i, "(Tables S7, S8, S9, S10, S14, S15, SM)", "(Tables S14 and S15, Figs S2-S6, SM)")

    # Discussion and limitations
    i = t.find_para("We meta-analysed 38 of these studies")
    t.replace_text(i, "We meta-analysed 38 of these studies",
                   f"We meta-analysed {N_DISTINCT} of these studies ({k_synth} sample-by-pathway contributions)")
    i = t.find_para("dissociation is the most consistent and strongest mediator")
    t.add_comment(i, "RT: Suggest softening 'strongest mediator'. Dissociation has the largest standardised indirect "
                     "effects (Table 2), but these are associations, percentages are not comparable across pathways, "
                     "and we have told Reviewer 1 we make no ranking of mechanisms. For example: 'dissociation showed "
                     "the most consistent and largest indirect associations ...'.")
    i = t.find_para("As shown in Table 2, all six meta-analysed pathways")
    t.replace_text(i, "all six meta-analysed pathways", "all five meta-analysed pathways")
    t.replace_text(i, "because only four to ten samples contributed", "because only five to ten samples contributed")
    i = t.find_para("Furthermore, most studies scored low on the NOS scale")
    t.add_comment(i, "RT for Anjali (Reviewer 3, study designs): in the quantitative subset (28 distinct samples, 34 "
                     "sample-by-pathway contributions), only one sample (Dhondt) supplied a correlation triplet in "
                     "which the mediator was measured before the outcome; the other longitudinal studies (Mertens, "
                     "LoPilato, Saladino) contributed same-wave correlations. Proposed sentence: 'In the quantitative "
                     "synthesis, only one of 28 samples supplied correlations in which the mediator was measured before "
                     "the outcome, so the pooled indirect effects describe largely cross-sectional associations.' The "
                     "count across all 117 studies is still needed. This coding was AI-assisted and awaits human "
                     "verification (study_design_tracker_coded_2026-11-25.xlsx).")

    # Conclusions
    i = t.find_para("This review provides a comprehensive update on the psychological and biological processes")
    t.insert_text_after(i, "occurring later in life.",
                        " These three were the only mediator families with sufficient data for quantitative "
                        "synthesis, which reflects data availability rather than a ranking of mechanisms.")

    # Figure 2 title and legend
    i = t.find_para("Figure 2:")
    assert t.para_text(t.paragraph_at(i)).strip() == "Figure 2:", "Figure 2 title paragraph is not empty"
    t.insert_text_after(i, "Figure 2:", " Population-average mediation quantities from the unmoderated primary "
                                        "Bayesian models")
    i = t.find_para("Points are posterior medians and horizontal lines are 95% equal-tailed credible intervals for the standardised a path")
    t.replace_text(i, "Points are posterior medians",
                   "The five pathways with at least five contributing samples are shown, with k, the number of "
                   "samples; other mediator families, including post-traumatic stress symptoms, had fewer than five "
                   "eligible samples and were not synthesised. Points are posterior medians")
    t.replace_text(i, "for the standardised a path, b path, direct effect c', indirect effect ab and total effect.",
                   "for the standardised a path (CA to mediator), b path (mediator to psychosis, holding CA constant), "
                   "direct effect c′ (CA to psychosis, holding the mediator constant), indirect effect ab (the product "
                   "of a and b) and total association (c′ + ab, equal to the CA–psychosis correlation). Because all are "
                   "standardised, ab = 0.10 means that a one standard deviation difference in CA is associated with a "
                   "0.10 standard deviation difference in psychosis through the mediator path. New-study prediction "
                   "intervals are shown in Fig S1 of the SM.")
    return t


def replace_math(t: TrackedDocx, idx: int, old: str, new: str) -> None:
    """Tracked replacement of a whole equation run: the m:r is wrapped in w:del
    and a copy carrying the new text is added in w:ins, inside the same oMath."""
    M = "http://schemas.openxmlformats.org/officeDocument/2006/math"
    hits = [r for r in t.paragraph_at(idx).iter("{%s}r" % M)
            if "".join(x.text or "" for x in r.iter("{%s}t" % M)) == old]
    assert len(hits) == 1, f"math run {old!r}: {len(hits)} matches"
    r = hits[0]
    import copy
    new_r = copy.deepcopy(r)
    ts = list(new_r.iter("{%s}t" % M)); ts[0].text = new
    for extra in ts[1:]: extra.getparent().remove(extra)
    parent, pos = r.getparent(), list(r.getparent()).index(r)
    d = t._del_wrap(r); parent.insert(pos, d); d.append(r)
    d.addnext(t._ins([new_r]))
    t.log.append(f"replace math in para {idx}: {old} -> {new}")


# ---- Table 2 -----------------------------------------------------------------
def table2() -> TrackedDocx:
    t = TrackedDocx(str(NBR / "Table 2 (2).docx"), date=DATE, date_utc=DATE_UTC)
    tbl = t.body.find(q("w:tbl"))
    rows = tbl.findall(q("w:tr"))
    cell_text = lambda tc: "".join(x.text or "" for x in tc.iter(q("w:t")))
    first = [cell_text(tr.findall(q("w:tc"))[0]).strip() for tr in rows]
    ORDER = {"Dissociation to hallucinations": "dissoc_hal", "Dissociation to delusions": "dissoc_del",
             "Negative schema to composite psychosis": "negsch_cps", "Negative schema to delusions": "negsch_del",
             "Depression to composite psychosis": "depress_cps"}
    assert first[0] == "Pathway" and "Dissociation to composite psychosis" in first

    def set_cell(tc, new):
        old = cell_text(tc)
        if old.strip() == new: return
        ps = [p for p in tc.findall(q("w:p")) if t.para_text(p).strip()]
        assert len(ps) == 1, f"cell {old!r} has {len(ps)} text paragraphs"
        p = ps[0]
        idx = t.paragraphs().index(p) if p in t.paragraphs() else None
        assert idx is None  # table paragraphs are not top-level; edit via a temporary shim
        _replace_in_para(t, p, old.strip(), new)

    for tr, name in zip(rows, first):
        tcs = tr.findall(q("w:tc"))
        if name == "Dissociation to composite psychosis":
            t.delete_table_row(tr); continue
        if name not in ORDER: continue
        r = P[ORDER[name]]
        if ORDER[name] in ("dissoc_hal", "dissoc_del"):
            vals = [None, str(r["k"]), n3(r["ab"], name + " T2 ab"),
                    ci(r["ab_crI_lo"], r["ab_crI_hi"], name + " T2 CrI"), pf(r["p_ab_gt0"], name + " T2 P"),
                    ci(r["ab_pi_lo"], r["ab_pi_hi"], name + " T2 PI"),
                    f"{n1(r['pct'], name + ' T2 pct')} [{n1(r['pct_crI_lo'], name + ' T2 pct lo')}, "
                    f"{n1(r['pct_crI_hi'], name + ' T2 pct hi')}]"]
            for tc, v in zip(tcs, vals):
                if v is not None: set_cell(tc, v)
        else:   # unchanged rows: check the printed values match the release
            exp = [str(r["k"]), f"{float(r['ab']):.3f}",
                   f"[{float(r['ab_crI_lo']):.3f}, {float(r['ab_crI_hi']):.3f}]"]
            got = [cell_text(tc).strip() for tc in tcs[1:4]]
            assert got == exp, (name, got, exp)

    # New columns: HDI after the CrI column, total association before the percentage.
    body = [tr for tr in tbl.findall(q("w:tr"))]
    names = [cell_text(tr.findall(q("w:tc"))[0]).strip() for tr in body]
    def col(fn, header):
        out = []
        for nm in names:
            if nm == "Pathway": out.append(header)
            elif nm in ORDER: out.append(fn(P[ORDER[nm]], nm))
            else: out.append("")     # the deleted row
        return out
    hdi = col(lambda r, nm: ci(r["ab_hdi_lo"], r["ab_hdi_hi"], nm + " T2 HDI"), "95% HDI")
    tot = col(lambda r, nm: f"{n3(r['total'], nm + ' T2 total')} "
                            f"{ci(r['total_crI_lo'], r['total_crI_hi'], nm + ' T2 total CrI')}",
              "Total association [95% CrI]")
    W_OLD = [2373, 630, 981, 2179, 1193, 2788, 3383]
    W_NEW = [1700, 520, 820, 1600, 950, 1750, 2237]   # + HDI 1700 + total 2250 = 13527
    t.insert_table_column(tbl, hdi, width=1700, widths=W_NEW, position=4)
    t.insert_table_column(tbl, tot, width=2250, position=7)
    grid = [int(g.get(q("w:w"))) for g in tbl.find(q("w:tblGrid")).findall(q("w:gridCol"))]
    assert sum(grid) == sum(W_OLD) == 13527, grid

    # Footnote
    i = t.find_para("Table 2 footnote/description:")
    t.replace_text(i, "ab is the standardised population-average indirect effect;",
                   "ab is the standardised population-average indirect effect, the product of a (the standardised "
                   "association between childhood adversity and the mediator) and b (the standardised association "
                   "between the mediator and psychosis, holding adversity constant); the total association is c′ + ab, "
                   "where c′ is the standardised association between adversity and psychosis holding the mediator "
                   "constant, and equals the adversity–psychosis correlation. All are associations on a standardised "
                   "scale: ab = 0.10 means that a one standard deviation difference in adversity is associated with a "
                   "0.10 standard deviation difference in psychosis through the mediator path;")
    t.replace_text(i, "CrI, equal-tailed credible interval;",
                   "CrI, equal-tailed credible interval (2.5% of the posterior excluded from each tail); HDI, "
                   "highest-density interval (the narrowest interval containing 95% of the posterior), which can "
                   "differ from the CrI at zero when the posterior lies close to it, as for negative schema to "
                   "composite psychosis, without the two being in conflict;")
    t.replace_text(i, "it is a statistical decomposition of pooled mediating effects.",
                   "it is a statistical decomposition of pooled associations, not a causal share, and is not directly "
                   "comparable across pathways. Only pathways with at least five contributing samples were "
                   "synthesised; dissociation to composite psychosis (four samples) was not.")
    t.replace_text(i, "HDI and additional details", "Additional details")
    t.replace_text(i, "Fig S11", "Fig S1")
    return t


def _replace_in_para(t: TrackedDocx, p, old: str, new: str) -> None:
    """replace_text for a paragraph that is not a top-level body paragraph (a
    table cell): temporarily expose it through paragraphs()."""
    orig = t.paragraphs
    t.paragraphs = lambda: [p]
    try:
        t.replace_text(0, old, new)
    finally:
        t.paragraphs = orig


# ---- Abstract ----------------------------------------------------------------
def abstract() -> TrackedDocx:
    t = TrackedDocx(str(NBR / "Abstract.docx"), date=DATE, date_utc=DATE_UTC)
    abstract_edits(t, comment=False)
    return t


if __name__ == "__main__":
    for name, fn in [("Neuroscience&BiobehavioralMAIN_ 10.26.docx", manuscript),
                     ("Table 2 (2).docx", table2), ("Abstract.docx", abstract)]:
        t = fn()
        t.save(str(OUT / name))
        print(f"{name}: {len(t.log)} edits")
        for l in t.log: print("   ", l)
    with open(OUT / "numbers_used.csv", "w", newline="") as f:
        w = csv.writer(f); w.writerow(["what", "source_value", "printed"]); w.writerows(USED)
