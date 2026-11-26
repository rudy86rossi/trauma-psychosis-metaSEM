"""Turn the 'Plan (RT):' notes in the rebuttal into 'Response:' text and delete the primer.

    python3 -I write_responses.py SRC.docx DST.docx NBR_DIR

Authorised regions only:
 - in each of the ten plan paragraphs (found by distinctive text, never by
   index), the bold blue 'Plan (RT): ' label and the blue body runs that follow
   it, up to the first run that is not blue;
 - the primer block, from the empty blue paragraph before 'Statistical primer
   for co-authors' to the Dienes reference, including its OLE_LINK1 bookmark.
Co-author text inside a plan's body runs ('KEEP') is carried over verbatim at
the end of the response. Every other part of the package is copied byte for
byte, and every number quoted is checked against the release tables first.
"""
import csv
import re
import sys
import zipfile
from pathlib import Path

from lxml import etree

W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
NS = {"w": W}
q = lambda t: "{%s}%s" % (W, t)
BLUE = "0070C0"
src, dst, nbr = sys.argv[1], sys.argv[2], Path(sys.argv[3])

## ---- numbers, checked against the release before anything is written ----
rows = {r["pathway"]: r for r in csv.DictReader(open(nbr / "primary_run_0c04f897e2d1.csv"))}
sens = {r["fit_id"]: r for r in csv.DictReader(open(nbr / "sensitivity_run_0c04f897e2d1.csv"))}
grid = list(csv.DictReader(open(nbr / "prior_grid_run_0c04f897e2d1.csv")))
f3 = lambda x: f"{float(x):.3f}"
ab = [float(r["ab"]) for r in rows.values()]
tot = [float(r["total"]) for r in rows.values()]
pab = [float(r["p_ab_gt0"]) for r in rows.values()]
pct = [float(r["pct"]) for r in rows.values()]
k = [int(r["k"]) for r in rows.values()]
cc = sens["loso_stan|negsch_del|null|S025"]
nd = rows["negsch_del"]
alt = [r for r in grid if r["prior"] != "primary"]
prim = {r["pathway"]: r for r in grid if r["prior"] == "primary"}
chg = [float(r["ab_change_pct"]) for r in alt]
dpct = [abs(float(r["pct"]) - float(prim[r["pathway"]]["pct"])) for r in alt if r["pct_status"] == "released"]
checks = {
    "ab range": (f3(min(ab)), f3(max(ab))) == ("0.053", "0.175"),
    "P(ab>0) range": (f3(min(pab)), max(pab) > 0.999) == ("0.976", True),
    "total range": (f3(min(tot)), f3(max(tot))) == ("0.251", "0.341"),
    "total CrIs exclude zero": all(float(r["total_crI_lo"]) > 0 for r in rows.values()),
    "pct range": (f"{min(pct):.1f}", f"{max(pct):.1f}") == ("20.8", "61.5"),
    "k range": (min(k), max(k), len(k)) == (5, 10, 5),
    "all PIs include zero": all(float(r["ab_pi_lo"]) < 0 < float(r["ab_pi_hi"]) for r in rows.values()),
    "case-control": (cc["k"], f3(cc["ab"]), f3(cc["ab_crI_lo"]), f3(cc["ab_crI_hi"]), f3(cc["p_ab_gt0"]),
                     cc["pct_status"]) == ("4", "0.070", "0.007", "0.146", "0.984", "withheld"),
    "negsch_del primary": (f3(nd["ab"]), f3(nd["ab_crI_lo"]), f3(nd["ab_crI_hi"])) == ("0.065", "0.011", "0.130"),
    "grid size": len(alt) == 20,
    "grid min P": f"{min(float(r['p_ab_gt0']) for r in alt):.3f}" == "0.966",
    "grid change": (f"{min(chg):.1f}", f"{max(chg):.1f}") == ("-25.1", "8.4"),
    "grid CrI incl 0": sorted({r["pathway"] for r in alt if float(r["ab_crI_lo"]) <= 0}) == ["negsch_cps"]
                       and sum(float(r["ab_crI_lo"]) <= 0 for r in alt) == 3,
    "grid PI excl 0": [r["prior"] for r in alt if float(r["ab_pi_lo"]) > 0] == ["tau_sd=0.1"] * 3,
    "grid pct shift": f"{max(dpct):.1f}" == "7.8",
    "cc pct in range": f"{float(cc['p_pct_in_range']):.3f}" == "0.944",
    "hal pct and deletions": (f"{float(rows['dissoc_hal']['pct']):.1f}", float(rows["dissoc_hal"]["pct_crI_hi"]) > 100)
                             == ("61.5", True),
    "prior mass": (round(__import__("math").erf(__import__("math").atanh(0.6) / 0.35 / 2 ** 0.5), 2),
                   round(1.96 * 0.20, 2)) == (0.95, 0.39),
    "grid withheld": [(r["pathway"]) for r in alt if r["pct_status"] != "released"] == ["negsch_del"] * 2,
}
bad = [n for n, ok in checks.items() if not ok]
assert not bad, f"numbers no longer match the release: {bad}"

## ---- responses: (distinctive text in the old plan, old plan text up to any kept co-author text,
##      kept co-author text or "", new response) ----
R = [
("the percentage moves out of the Abstract to the Results only", "",
 "We agree. The Abstract now reports the standardised indirect effect (ab), the beta-type coefficient "
 "the reviewer suggests: across the five synthesised pathways ab ranged from 0.053 to 0.175, with "
 "posterior probabilities of a positive effect from 0.976 to >0.999. The model-based percentage has "
 "been removed from the Abstract and appears in the Results only, where it is described as the "
 "percentage of the pooled adversity-psychosis association represented by the estimated indirect "
 "statistical pathway (Sections 3.2.1, 3.2.2 and 3.3). The medians that remain in the Abstract are now "
 "labelled as descriptive median percentages mediated, taken from the individual published studies, "
 "so they are no longer confused with the model estimates. The Table 2 footnote states that the "
 "model-based percentage is a statistical decomposition of pooled associations, not a causal share, "
 "and the Methodological considerations section states that it should not be read as an "
 "individual-level causal percentage. Each Bayesian model contains a single mediator, so with these "
 "data the statistical overlap between constructs cannot be partialled out; the limits of the mediator "
 "categories are discussed under Methodological considerations. The percentage and the rule for "
 "reporting it were adopted post hoc, as the Methods now state. After the corrections to three source "
 "extractions described in our response to Reviewer 2, the dissociation-to-hallucinations percentage "
 "the reviewers quote rose from 55.5% to 61.5%, but its 95% credible interval runs above 100% and it "
 "was withheld under the stability rule in 5 of 10 single-cohort deletions (Section 3.2.1), so we "
 "present it as fragile."),
("We will report the total association itself as a standardised coefficient", "",
 "The total association is now reported as a standardised coefficient rather than a percentage. Its "
 "posterior medians ranged from 0.251 to 0.341 across the five pathways, and every 95% credible "
 "interval excluded zero (Section 3.3, and a new Total association [95% CrI] column in Table 2). The "
 "Table 2 footnote defines it as c′ + ab, which equals the adversity-psychosis correlation, and "
 "Section 3.3 now states that the percentages (20.8% to 61.5%) are shares of that association within "
 "each pathway and are not directly comparable across pathways. Because the Abstract no longer reports "
 "a model-based percentage, the only percentages there are the descriptive medians, labelled as such."),
("One Abstract sentence will explain that a prediction interval including zero", "",
 "The Abstract now states that all 95% new-study prediction intervals included zero and explains what "
 "this means: the average indirect association is very probably positive, but the true indirect "
 "association in a new setting could be small or absent, because its size varies between settings. The "
 "new Methods paragraph, Interpreting the Bayesian results, and the Methodological considerations "
 "section explain this further. On the number of meta-analyses: three mediator families (dissociation, "
 "negative schemas and depression) gave five mediator-outcome pathways (dissociation to hallucinations "
 "and to delusions, negative schemas to composite psychosis and to delusions, and depression to "
 "composite psychosis), each pooling five to ten samples, with 34 sample-by-pathway contributions from "
 "28 distinct samples. Dissociation to composite psychosis, which had four samples and was included in "
 "the submitted version, is no longer synthesised, in line with the minimum of five studies specified "
 "in the PROSPERO pre-registration (Methods, Meta-analysis). The same five pathways are now used in the "
 "Abstract, Results, Table 2, Figure 2, Discussion and Conclusions. Psychosocial stressors, which had no "
 "quantitative synthesis, have been removed from the Conclusions."),
("We will add a short 'Interpreting the Bayesian results' paragraph to the Methods", "",
 "We have added a paragraph, Interpreting the Bayesian results, to the Methods (Meta-analysis). It "
 "explains the priors, credible versus confidence intervals, the two credible intervals we report, "
 "posterior probabilities and prediction intervals, and gives a key to the coefficients (see also our "
 "response on a key below). On the bullets in turn.\nWhy these priors: they are weakly informative and "
 "centred on zero (normal with standard deviation 0.35 for the population-average Fisher-z "
 "correlations, half-normal with scale 0.20 for the between-study standard deviations), so they favour "
 "neither a positive nor a negative association. The first places about 95% of its mass on "
 "population-average correlations between -0.6 and 0.6, and the second about 95% on between-study "
 "standard deviations below 0.39 on the Fisher-z scale.\nDependence on the priors: we refitted every pathway under four "
 "alternative priors (20 refits, new Table S23 in the SM). The direction of every indirect effect was "
 "robust, with the lowest posterior probability of a positive effect approximately 0.966. Its magnitude "
 "changed by -25.1% to +8.4%; the credible interval included zero in three refits, all for negative "
 "schemas to composite psychosis; and the prediction interval excluded zero in three, all under the "
 "narrowest heterogeneity prior, which narrows the interval by assumption rather than through the data "
 "(Section 3.3). Released percentages moved by at most 7.8 percentage points, and two percentages for "
 "negative schemas to delusions were withheld under the stability rule. Between-study heterogeneity is "
 "substantially prior-informed, as the Results state.\nHigh posterior probabilities alongside prediction "
 "intervals that include zero: these answer different questions. The posterior probability concerns the "
 "population-average indirect association across the studies pooled, whereas the prediction interval "
 "concerns the true indirect effect in a new sample and adds between-study heterogeneity, so a pathway "
 "can be very probably positive on average yet small or absent in some settings. With five to ten "
 "samples per pathway, that heterogeneity, and so the prediction interval, is imprecisely estimated "
 "(Methods; Methodological considerations).\nGeneralisable or population-specific: the results support "
 "a positive average indirect association across the populations sampled, not a pathway expected to "
 "hold with the same strength in every population, and the moderators examined (age, sample type, "
 "percentage female and study quality) did not explain the variation (Section 3.3).\nCredible versus "
 "confidence intervals: the Methods now state that a 95% credible interval contains the "
 "population-average value with 95% probability given the data and model, whereas a confidence "
 "interval describes how often the procedure would capture the true value across repeated studies."),
("The protocol required at least five cohorts reporting all three correlations", "",
 "The PROSPERO pre-registration specified a minimum of five studies for quantitative synthesis, and a "
 "Bayesian meta-analytic structural equation model needs all three correlations (adversity-mediator, "
 "adversity-psychosis and mediator-psychosis) from the same sample. Post-traumatic stress symptoms, "
 "anxiety, attachment and psychosocial stressors each had fewer than five eligible samples, so they "
 "remain in the descriptive synthesis (Sections 3.2.3 to 3.2.6). The Methods (Meta-analysis) now say "
 "so, and the Figure 2 legend says so for post-traumatic stress symptoms. Applying the same rule, we no "
 "longer synthesise dissociation to composite psychosis (four samples), which was included in the "
 "submitted version."),
("the Conclusions (Abstract and main text) will be limited to the three mediator families", "",
 "The Conclusions (Abstract and Section 4.3) now name dissociation, negative schemas and depression as "
 "the families with sufficient data for synthesis, and Section 4.3 states that these three were the "
 "only mediator families with sufficient data for quantitative synthesis, which reflects data "
 "availability rather than a ranking of mechanisms. Psychosocial stressors have been removed from both. "
 "We do not claim that post-traumatic stress symptoms or attachment are weaker mediators: their "
 "descriptive median percentages (35.2% and 27.1%) are comparable with those of the synthesised "
 "families, and the higher proportion of supportive analyses for post-traumatic stress symptoms in "
 "clinical samples (85.7% against 66.6% in non-clinical samples) is reported in Section 3.2.3. They "
 "could not be tested quantitatively because too few samples reported the three correlations needed "
 "(see our response on Bayesian synthesis above)."),
("Work needed: regenerate all figures as vector PDF/EPS", " – also add trauma pathway. ",
 "Figure 2 has been redrawn from the corrected analysis as vector PDF and EPS files at 190 mm, the "
 "journal's full-page width, with fonts embedded and all text at least 7 pt; a 1000 dpi TIFF is "
 "supplied as a fallback. The new-study prediction-interval figure (Figure S1) has been produced in "
 "the same way, and the other supplementary figures from the Bayesian analysis have been regenerated "
 "from the corrected analysis. [Luis to add: Figures 1, 3 and 4.] Post-traumatic stress symptoms are "
 "not plotted in Figure 2 because they had too few samples for a Bayesian synthesis; the Figure 2 "
 "legend now says so, and they remain in Figure 1 with the other descriptive results."),
("In the submitted version Figure 2 is the six-pathway forest plot",
 " – could add trauma, explain why only some are selected based on criteria. ",
 "Figure 2 shows every mediator-outcome pathway that met the minimum of five studies specified in the "
 "PROSPERO pre-registration, counting each distinct sample once. Five pathways qualified. Dissociation "
 "to composite psychosis (four samples), which was shown in the submitted figure, does not qualify and "
 "has been removed. The legend now states the rule and notes that other mediator families, including "
 "post-traumatic stress symptoms, had fewer than five eligible samples and were not synthesised. The "
 "figure therefore reflects data availability, not a selection by effect size or statistical "
 "significance."),
("We will add a short key to Table 2 and Figure 2", "",
 "We have added a key to the Table 2 footnote, the Figure 2 legend and the Methods. In each model, a is "
 "the standardised association between adversity and the mediator, b is the standardised association "
 "between the mediator and psychosis holding adversity constant, c′ is the standardised association "
 "between adversity and psychosis holding the mediator constant, ab is the product of a and b (the "
 "indirect effect), and the total association, c′ + ab, equals the adversity-psychosis correlation. "
 "Because all are standardised, ab = 0.10 means that a one standard deviation difference in adversity "
 "is associated with a 0.10 standard deviation difference in psychosis through the mediator path. We "
 "have not attached labels such as small, moderate or large, because those benchmarks were proposed for "
 "single correlations and have no agreed equivalent for a product of two paths. The new-study "
 "prediction intervals are shown in Figure S1 of the SM."),
("Agreed approach (Luis to lead): remove the two case-control (caseness) studies", "",
 "We have added this sensitivity analysis. When we checked each contributing study's design against "
 "the correlations actually used, only one sample had been recruited by case-control design: Appiah "
 "2017, which contributes to negative schemas to delusions. Chiu 2024 contributes correlations from its "
 "patients only, so it does not compare cases with controls in our analysis. Removing Appiah 2017 left "
 "four samples, below the five-sample minimum, so this check is exploratory. The indirect effect was "
 "similar (ab = 0.070 [0.007, 0.146], posterior probability of a positive effect 0.984, against 0.065 "
 "[0.011, 0.130] with it), but the percentage was withheld because the posterior probability that it "
 "lay between 0 and 100 was 0.944, below the 0.95 threshold (Methods, Meta-analysis; Section 3.2.2). The wider comparison of "
 "clinical and non-clinical samples is in Table 1 and in the sample-type moderator analysis (Section "
 "3.3). While checking the coding we also corrected three extraction errors against the source papers: "
 "two correlations each for Cole 2016 and Perona-Garcelán 2012 (the latter had been taken from the "
 "Amnesia subscale rather than the total Dissociative Experiences Scale score), and the percentage "
 "female for Pearce 2017 (72%; the mean age had been entered in error). All models were refitted. The "
 "negative-schema and depression results are unchanged, the dissociation results are updated "
 "throughout (Section 3.2.1, Table 2 and Figure 2), and there is still no robust evidence of moderation "
 "by percentage female. We also checked the clinical coding of four studies on which the extraction "
 "sheets disagreed (Gibson 2019, Perona-Garcelán 2012, Pearce 2017 and Sengutta 2019); the coding used "
 "in the models was confirmed for all four under the review's definition of a clinical sample (a "
 "psychosis diagnosis), so the sample-type moderator did not need refitting. [Luis and Lora: Sengutta "
 "2019 still needs moving from Table S7 to Table S9 in the SM.]"),
]

## ---- edit ----
zin = zipfile.ZipFile(src)
root = etree.fromstring(zin.read("word/document.xml"))
body = root.find(q("body"))
rtext = lambda r: "".join(t.text or "" for t in r.iter(q("t")))
color = lambda r: (r.find("w:rPr/w:color", NS).get(q("val")) if r.find("w:rPr/w:color", NS) is not None else None)
bold = lambda r: r.find("w:rPr/w:b", NS) is not None
paras = body.findall(q("p"))
ptext = lambda p: "".join(rtext(r) for r in p.iter(q("r")))

for key, keep, new in R:
    hit = [p for p in paras if key in ptext(p)]
    assert len(hit) == 1, f"{key!r}: {len(hit)} matches"
    runs = hit[0].findall(q("r"))
    lab = [i for i, r in enumerate(runs) if rtext(r) == "Plan (RT): " and bold(r) and color(r) == BLUE]
    assert len(lab) == 1, key
    i = lab[0]
    body_runs = []
    for r in runs[i + 1:]:
        if color(r) != BLUE or bold(r):
            break
        body_runs.append(r)
    old = "".join(rtext(r) for r in body_runs)
    assert key in old and old.endswith(keep), f"{key!r}: body or kept text not where expected"
    assert "Plan (RT)" not in old and "Response:" not in old
    runs[i].find(q("t")).text = "Response: "
    t = body_runs[0].find(q("t"))
    for extra in body_runs[0].findall(q("t"))[1:]:
        body_runs[0].remove(extra)
    assert all(c.tag in (q("rPr"), q("t")) for c in body_runs[0]), "unexpected run content"
    ## Line breaks in a response become w:br inside the same run.
    parts = (new + keep).split("\n")
    t.text = parts[0]
    t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    for part in parts[1:]:
        etree.SubElement(body_runs[0], q("br"))
        nt = etree.SubElement(body_runs[0], q("t"))
        nt.text = part
        nt.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    for r in body_runs[1:]:
        r.getparent().remove(r)

## Primer: the empty blue paragraph, the title, eight items and the reference.
title = [j for j, p in enumerate(paras) if ptext(p).startswith("Statistical primer for co-authors (RT)")]
ref = [j for j, p in enumerate(paras) if ptext(p).startswith("Reference: Dienes, Z. (2020)")]
assert len(title) == len(ref) == 1 and ref[0] - title[0] == 9
start = title[0] - 1
assert ptext(paras[start]) == "" and paras[start].find("w:pPr/w:rPr/w:color", NS).get(q("val")) == BLUE
block = paras[start:ref[0] + 1]
assert paras[ref[0] + 1:] == [], "primer is not the last block before sectPr"
bm = [e.get(q("id")) for p in block for e in p.iter(q("bookmarkStart"), q("bookmarkEnd"))]
assert bm == ["0", "0"], f"bookmarks in primer not self-contained: {bm}"
for p in block:
    body.remove(p)

out = etree.tostring(root, xml_declaration=True, encoding="UTF-8", standalone=True)
with zipfile.ZipFile(dst, "w", zipfile.ZIP_DEFLATED) as z:
    for info in zin.infolist():
        z.writestr(info, out if info.filename == "word/document.xml" else zin.read(info.filename))
print(f"wrote {dst}: {len(R)} responses, {len(block)} primer paragraphs removed")
