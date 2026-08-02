# Sample identity: source verification dossier

British English throughout.

**Prepared:** 31 July 2026.
**Prepared by:** AI-assisted source check (Claude Opus 5), from published full texts,
Europe PMC records and three papers held locally as PDF. This research was
prepared by a machine and is labelled as such wherever it is relied on.
**Accepted by:** R Twumasi, 31 July 2026, having read the row-level evidence
below. Nine mapping rows were accepted as `evidenced` and one, Uyan 2022, as
`disputed`. The remaining 31 stay `unevidenced`. No row was written to
`data/provenance/sample_map.csv` before that acceptance.

## What this document is for

`data/provenance/sample_map.csv` asserts, for each of the 41 eligible effect
rows, which participant cohort supplied it. The assertions currently in the file
were seeded mechanically from the previous author-year derivation, so they
reproduce exactly what the pipeline did before, and every one of them is
`unevidenced`. This dossier records what was found when those assertions were
checked against their sources.

## The evidence standard

An assertion may be marked `evidenced` only on the strength of a full-text page,
table or recruitment passage, a registry record, or correspondence. A DOI, an
abstract, and a participant count that happens to match are **not** sufficient:
none of them establishes that two reports describe the same people, or that one
report describes two cohorts, which is the only question the map exists to
answer. A matching participant count is recorded below as corroboration and is
never the evidence.

## Structural findings that need no source

These are facts about the extraction workbooks, established computationally.
They constrain where a mistake could be hiding; they are not evidence about
cohorts.

- The 41 eligible effect rows occupy 41 distinct source rows. Every workbook row
  feeds exactly one pathway, so no row is double-counted across pathways.
- All 32 study labels remain distinct after stripping case and punctuation, so
  none of the four punctuation flags in the review queue is a hidden collision
  with another cohort. `Sun, 2018`, `Perona‚ 2012`, `Varese, 2012` and
  `Uyan, 2022` are untidy labels, not ambiguous ones.
- No two cohorts share a first-author label in any year, so no accidental split
  or merge can be hiding behind adjacent-year reports by one author.
- Only two cohorts share a participant count, Cole 2016 and Sengutta 2019, both
  n = 200. They are different authors, years, workbooks and pathways, and the
  coincidence carries no implication.
- One cohort spans two source workbooks: S013, Gibson 2019. That is the single
  genuine structural question in the set, and it is answered below.

## Cohorts checked against full text

Eight cohorts, ten mapping rows. Each entry states the source, the exact
locator, what it establishes, and the corroborating participant count.

### S013 Gibson 2019, rows M020 (dissoc_cps) and M021 (negsch_cps)

**This is the one that mattered.** The review queue asked whether the
dissociation report and the negative-schema report describe the same
participants. They do.

- **Source:** Gibson LE, Reeves LE, Cooper S, Olino TM, Ellman LM (2019).
  Traumatic life event exposure and psychotic-like experiences: a multiple
  mediation model of cognitive-based mechanisms. *Schizophrenia Research*.
  doi:10.1016/j.schres.2018.02.005. Open full text at PMC6098745.
- **Locator:** Method, Participants and Measures.
- **Establishes:** "Undergraduate students (N = 945) between the ages of 18 and
  34 were from a socioeconomically and racially diverse urban university in the
  United States." The Brief Core Schema Scale and the Dissociative Experiences
  Scale were both administered to that one sample, within a single multiple
  mediator model; "after listwise deletion of missing data, the multiple
  mediator model included 941 participants."
- **Corroboration:** registry n = 945, matching the recruited N.
- **Consequence:** S013 is correctly one cohort contributing one triplet to each
  of two pathways. No split is warranted, and the cross-workbook review item is
  answered rather than merely noted.
- **Proposed status:** `evidenced`, `fulltext`.

### S014 Blose 2023, rows M022 (PDEQ) and M023 (DES-II)

- **Source:** Blose BA, Godleski SA, Houston RJ, Schenkel LS (2023). The
  indirect effect of peritraumatic dissociation on the relationship between
  childhood maltreatment and schizotypy. *Journal of Interpersonal Violence*.
  doi:10.1177/08862605221122832. Full text held locally as PDF.
- **Locator:** p. 6 recruitment; p. 9 final sample; Table 2, p. 10.
- **Establishes:** "Participants were N = 356 undergraduates from a private
  Northeastern technological university"; after deletion for missing data and
  inattentive responding and exclusion of participants over the age limit,
  "leaving a final sample size of N = 346". Table 2 is a single correlation
  matrix over CTQ-SF, PDEQ, DES II and SPQ, so both dissociation instruments
  were administered to the same 346 participants.
- **Corroboration:** registry n = 346.
- **Consequence:** confirms the `alternative_instrument` declaration. Two
  instruments, one cohort, one triplet with the other retained as a replacement
  sensitivity analysis.
- **Proposed status:** `evidenced`, `fulltext`.

### S011 Mertens 2021, row M018 (dissoc_del)

- **Source:** Mertens YL et al. (2021). Dissociation and insecure attachment as
  mediators of the relation between childhood emotional abuse and nonclinical
  paranoid traits. Open full text at PMC8079066.
- **Locator:** Methods, section 2.1 Participants.
- **Establishes:** "The participants (N = 89) were drawn from a sample of
  undergraduate students from the Universitat Autònoma de Barcelona recruited
  via the university participation system, who participated in an ongoing
  longitudinal research investigating vulnerability and protective factors."
- **Corroboration:** registry n = 89.
- **Note for the reviewer:** the analysed 89 are a selected subsample of a larger
  longitudinal cohort (214 assessed at T2, selected on standard scores). This is
  one analytical cohort, but if any other included study draws on the same
  Barcelona longitudinal panel, that would be an overlap this dossier has not
  ruled out. No other included study is Barcelona-based on its face.
- **Proposed status:** `evidenced`, `fulltext`.

### S024 Hardy 2016, row M033 (negsch_del)

- **Source:** Hardy A et al. (2016). Psychological mechanisms mediating effects
  between trauma and psychotic symptoms. *Schizophrenia Bulletin*.
  doi:10.1093/schbul/sbv175. Open full text at PMC4960432.
- **Locator:** Results, Sample.
- **Establishes:** "The PRP sample consisted of 301 participants. Two hundred
  twenty-eight (76% of the total) completed the THQ and were included in the
  current study." Negative schemas measured with the Brief Core Schema Scales.
- **Corroboration:** registry n = 228.
- **Note for the reviewer:** this is a secondary analysis of the PRP trial. Any
  other included study analysing PRP participants would overlap. None of the
  other 31 is obviously a PRP analysis, but that has been checked only by title.
- **Proposed status:** `evidenced`, `fulltext`.

### S020 Jaya 2017, row M029 (negsch_cps)

- **Source:** Jaya ES, Ascone L, Lincoln TM (2017). Social adversity and
  psychosis: the mediating role of cognitive vulnerability. *Schizophrenia
  Bulletin*. doi:10.1093/schbul/sbw104. Open full text at PMC5463978.
- **Locator:** Methods, Participants.
- **Establishes:** one integrated multinational crowdsourced sample, Germany
  n = 786, Indonesia n = 844, United States n = 720, from 2,501 submissions less
  151 duplicate or inconsistent entries, giving a final analysed N = 2,350. The
  Brief Core Schema Scales were administered to that single sample.
- **Corroboration:** registry n = 2350.
- **Note for the reviewer:** this cohort is itself a pooling of three national
  subsamples. It is one analytical unit as extracted, but it is worth
  remembering when the ecological nature of the study-level moderators is
  discussed.
- **Proposed status:** `evidenced`, `fulltext`.

### S018 Dhondt 2022, row M027 (negsch_cps)

- **Source:** Dhondt N et al. (2022). Childhood adversity and recurrence of
  psychotic experiences during adolescence. *Psychological Medicine*.
  doi:10.1017/S003329172200071X. Open full text at PMC10317802.
- **Locator:** Methods, participants and measures.
- **Establishes:** the sample is Cohort '98 of the Growing Up in Ireland study,
  initial sample 8,658 children, with 6,039 children and their families
  participating at all three waves (ages 9, 13 and 17); one longitudinal sample.
  Self-concept measured with the 60-item Piers-Harris scale, which is the
  negative-schema instrument recorded in the workbook.
- **Corroboration:** registry n = 6039.
- **Proposed status:** `evidenced`, `fulltext`.

### S015 Evans 2015, row M024 (dissoc_cps, excluded row)

- **Source:** Evans GJ, Reid G, Preston P, Palmier-Claus J, Sellwood W (2015).
  Trauma and psychosis: the mediating role of self-concept clarity and
  dissociation. *Psychiatry Research* 228:626-632.
  doi:10.1016/j.psychres.2015.04.053. Full text held locally as PDF.
- **Locator:** p. 2 recruitment; p. 6 group sizes; p. 4 and p. 5 aggregate
  sample.
- **Establishes:** clinical participants were recruited from five Early
  Intervention in Psychosis teams and non-clinical participants from a pool of
  adult learners; "only 31 non-clinical and 29 clinical participants took part in
  the research"; the mediation analyses were "carried out on pooled data from
  across both clinical and non-clinical samples", reported as the aggregate
  sample N = 60.
- **Corroboration:** registry n = 60, matching the aggregate.
- **Note for the reviewer:** the analysed unit is an aggregate of two separately
  recruited groups. The row is excluded permanently on independent grounds, the
  estimand being regression coefficients on binary group membership rather than a
  correlation triplet, so this does not affect any estimate. It is recorded
  because the registry still declares the cohort.
- **Proposed status:** `evidenced`, `fulltext`, with the aggregation noted.

### S016 Uyan 2022, row M025 (dissoc_cps, excluded row)

- **Source:** Uyan TT, Baltacioglu M, Hocaoglu C (2022). Relationships between
  childhood trauma and dissociative, psychotic symptoms in patients with
  schizophrenia: a case-control study. *General Psychiatry*.
  doi:10.1136/gpsych-2021-100659. Full text held locally as PDF, also open at
  PMC8796255.
- **Locator:** p. 2, Subjects and study design.
- **Establishes:** "The study was completed with a total of 100 patients. The
  control group consisted of 100 healthy participants ... Overall, 100 patients
  (51 women and 49 men) and 100 healthy controls (65 women and 35 men)." The
  report therefore describes **two** distinct recruited groups, and the extracted
  correlations come from a Figure 2 case-control mediation across the combined
  N = 200 while the registry records n = 100.
- **Corroboration:** the registry n = 100 matches the patient group only, not the
  analysed 200. This is a genuine disagreement, not a match.
- **Consequence:** this cohort is not well defined as a single participant
  sample. The row is already excluded as `pending_source_query`, so no estimate
  is affected, and the exclusion is now supported by the source rather than
  inferred.
- **Proposed status:** `disputed`, `fulltext`. Reinstatement would require the
  patient-only CTQ-to-DES correlation and a decision about which group the
  cohort denotes.

## Cohorts not checked against full text

Twenty-four cohorts, thirty-one mapping rows, remain `unevidenced`. For each, a
published record was located and the citation reconciled, but the full text is
behind a paywall and no admissible passage could be read. Locating a record is
not evidence about participants, so none of these is proposed for a change.

| Cohort | Report | Record located | Why not evidenced |
| --- | --- | --- | --- |
| S001 | Cole 2016 | PMID 27046664 | paywalled |
| S002 | Yanasaki 2016 | not located | the label may be a transliteration variant; needs the reviewer's own reference list |
| S003 | Bellido-Zanin 2018 | PMID 29808908 | paywalled |
| S004 | Gomez & Freyd 2017 | doi:10.1080/10538712.2017.1310776, PMID 28569650 | citation reconciled from the abstract only; abstract is not admissible |
| S005 | Jones 2023 | not located | the automated match was a different Jones paper and was rejected |
| S006 | Chiu 2024 | not located | the automated match was a different Chiu paper and was rejected |
| S007 | Sun 2018 | PMID 29694935 | paywalled |
| S008 | Perona 2012 | not located | automated matches were unrelated papers and were rejected; likely Perona-Garcelán |
| S009 | Varese 2012 | not located | the automated match was the 2012 meta-analysis, not the primary study with n = 45 |
| S010 | Pearce 2017 | PMID 28653442 | paywalled |
| S012 | Berenbaum 2008 | PMID 18729605 | paywalled |
| S017 | Boyda 2018 | doi:10.1002/jclp.22690 | paywalled |
| S019 | Dudley 2023 | doi:10.1093/schizbullopen/sgad030 | record located but the title returned does not obviously match a negative-schema mediation study; needs the reviewer to confirm the intended reference |
| S021 | Murphy 2015 | doi:10.1111/bjc.12077 | paywalled |
| S022 | Simpson 2020 | doi:10.1111/bjc.12245 | paywalled |
| S023 | LoPilato 2021 | PMID located, title on childhood adversity and cognitive schemas | paywalled |
| S025 | Appiah 2017 | doi:10.1016/j.jpsychires.2017.01.003 | paywalled; note the first author indexes as Appiah-Kusi |
| S026 | Ashford 2012 | doi:10.5127/jep.020611 | not indexed in Europe PMC; automated matches were unrelated and rejected |
| S027 | Pinto-Gouveia 2014 | doi:10.1002/cpp.1818 | paywalled |
| S028 | Østefjells 2017 | doi:10.1017/S0033291717000848 | paywalled |
| S029 | Steenkamp 2022 | doi:10.1016/j.schres.2019.03.021 | paywalled; workbook year 2022 against a 2019 article DOI, worth a look |
| S030 | Sengutta 2019 | doi:10.1016/j.eurpsy.2018.11.009 | paywalled |
| S031 | Mętel 2020 | doi:10.1111/eip.12829 | paywalled |
| S032 | Saladino 2025 | doi:10.1111/papt.70017 | paywalled; indexed publication year 2026 against a workbook year of 2025 |

## What this means for the manuscript

The eight proposed entries were accepted. Nine of 41 assertions are now
`evidenced` and one is `disputed`; the remaining 31 are not. The analytical unit therefore remains, for the
release as a whole, a derived author-year grouping that has not been externally
verified, and the Methods, abstract, results and discussion must say so. The
generated caveat and the document verifier enforce this: the sentence printed in
`BAYESIAN_METAREG_METHODS.md` states the evidenced count, and an unqualified
independence claim anywhere in either document fails the build.

What does change is the one thing that could have altered the estimates. Gibson
2019 was the only case where the data could have been wrong about how many
cohorts there are, and the source establishes that the current treatment is
correct. No split or merge is warranted, so no analytical unit changes, `data_sha`
is unchanged, and the audited estimates remain valid as regression benchmarks.

## What would raise the evidenced count

Institutional full-text access to the 24 paywalled reports, an hour of a
co-author's time with the reference list to resolve S002, S005, S006, S008,
S009, S019 and S026, and, where a report is ambiguous about its analysed sample,
correspondence with its authors. Each row resolved that way is a one-line edit to
`sample_map.csv` and requires no code change.
