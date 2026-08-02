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

**Second pass, 1 August 2026.** Full-text PDFs for all 24 remaining cohorts
were supplied (`psychosis_mediation_meta/literature/`), removing the paywall
that had blocked the first pass, and resolving the seven cases where the first
pass's automated search had found no record or the wrong paper (Yanasaki,
Jones, Chiu, Perona, Varese, Dudley, Ashford). Prepared by AI-assisted source
check (Claude Sonnet 5), under standing instruction to auto-accept unambiguous
matches and to raise only disputed or ambiguous cases for individual review;
accepted by R Twumasi under that same standing instruction, 1 August 2026. All
24 cohorts (31 rows) resolved to `evidenced`; none was disputed. See "Second
verification pass" below.

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

## First pass: cohorts not checked against full text (superseded below)

Kept for the record. Twenty-four cohorts, thirty-one mapping rows, were
`unevidenced` after the first pass. For each, a published record was located
and the citation reconciled, but the full text was behind a paywall and no
admissible passage could be read, or (seven cases) no record, or the wrong
record, had been found at all. Locating a record is not evidence about
participants, so none of these was proposed for a change at the time. All 24
were subsequently checked against full text in the second pass; see below.

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

## Second verification pass (1 August 2026): all remaining cohorts checked

All 24 cohorts below were checked against full-text PDFs now held in
`psychosis_mediation_meta/literature/`. Every one resolves to `evidenced`; none
is disputed. Several carry a note worth keeping even though they don't change
the proposed status: a registry label typo, an instrument that was blank in
the registry and is now identified, a two-study source paper where only one
study matches, and three publication-year discrepancies that all turn out to
be online-first-versus-print-issue artefacts rather than wrong-paper matches.

### S001 Cole 2016, rows M001 (dissoc_del) and M002 (dissoc_hal)

- **Source:** Cole, Newman-Taylor & Kennedy (2016). *Journal of Trauma &
  Dissociation*, 17(5), 577–592.
- **Locator:** p. 580 (PDF p. 5), Method, Participants.
- **Establishes:** "Two hundred undergraduate students... from a university in
  the United Kingdom formed a convenience sample." The DES-II, LSHS-R and PDI
  were all administered to this one sample (N = 200, N = 197 for the DES-II
  subscale owing to missingness).
- **Corroboration:** registry n = 200.
- **Consequence:** one cohort feeding both the hallucination-proneness (LSHS-R)
  and delusional-ideation (PDI) pathways, as the registry already asserts.
- **Proposed status:** `evidenced`, `fulltext`.

### S002 Yamasaki 2016 (registry label "Yanasaki 2016"), row M003 (dissoc_hal)

- **Source:** Yamasaki S, Ando S, Koike S, Usami S, Endo K, French P, Sasaki T,
  Furukawa TA, Hasegawa-Hiraiwa M, Kasai K, Nishida A (2016). *Schizophrenia
  Research: Cognition*, 4, 18–23.
- **Locator:** p. 19 (PDF p. 2), Methods, 2.2 Participants.
- **Establishes:** the Tokyo Early Adolescence Survey, a population-based,
  resident-register sample: 4,478 of 14,553 selected pairs participated; 201
  excluded for incomplete responses, "the remaining 4277 cases were analyzed."
  Dissociation was assessed by parent report on 6 CBCL items, matching the
  registry's CBCL instrument.
- **Corroboration:** registry n = 4277, exact match.
- **Consequence:** the registry studyID "Yanasaki" is a one-letter
  transliteration slip of "Yamasaki" (confirmed by the running head "S.
  Yamasaki et al." throughout the paper); it is the same study and the same
  single cohort. The studyID spelling in `sample_map.csv` should be corrected
  as a follow-up (not done in this pass, which only writes evidence columns).
- **Proposed status:** `evidenced`, `fulltext`.

### S003 Bellido-Zanin 2018, rows M004 (dissoc_del) and M005 (dissoc_hal)

- **Source:** Bellido-Zanin, Perona-Garcelán, Senín-Calderón, López-Jiménez,
  Ruiz-Veguilla & Rodríguez-Testal (2018). *Scandinavian Journal of
  Psychology*, 59, 407–413.
- **Locator:** p. 407 (PDF p. 2–3), Method, Participants.
- **Establishes:** "leaving a sample of 472 participants" of an original 495
  students at the Universities of Seville and Cádiz, after 23 disqualified for
  psychiatric treatment. The Spanish DES-II was administered to this one
  sample alongside the LSHS-R and the Referential Thinking Scale (REF).
- **Corroboration:** registry n = 472.
- **Consequence:** one cohort feeding both pathways. Note for the record: the
  paper's "delusion-adjacent" outcome is the REF ideas-of-reference scale, not
  a PDI-style delusion measure; the paper itself distinguishes the two. This
  does not create a second cohort, only a precision note on the dissoc_del
  outcome construct.
- **Proposed status:** `evidenced`, `fulltext`.

### S004 Gomez & Freyd 2017, row M006 (dissoc_hal)

- **Source:** Gómez & Freyd (2017). *Journal of Child Sexual Abuse*, 26(5),
  507–518.
- **Locator:** p. 509–510 (PDF p. 4–5), Method, Participants and procedure.
- **Establishes:** 202 of a 1,266-strong university subject pool volunteered;
  "resulting in a final sample size of N = 192" after excluding 10 for missing
  data. The Curious Experiences Survey (CES) was administered to this sample,
  matching the registry instrument.
- **Corroboration:** registry n = 192.
- **Consequence:** one recruited sample with ordinary volunteer attrition; the
  1,266 figure is the sampling frame, not a second cohort.
- **Proposed status:** `evidenced`, `fulltext`.

### S005 Jones 2023, row M007 (dissoc_hal)

- **Source:** Jones, Hughes-Ruiz & Vass (2023/2024). *Psychosis: Psychological,
  Social and Integrative Approaches*, 16(3), 233–242.
  doi:10.1080/17522439.2023.2254810.
- **Locator:** p. 4, Method, Participants.
- **Establishes:** "Participants included volunteers from the general
  population (N = 227)... or first-year psychology undergraduates," recruited
  via social media and the University of Liverpool's EPR scheme. The "revised
  dissociative experiences scale (DES... revised)" was administered, matching
  the registry's DES-R.
- **Corroboration:** registry n = 227.
- **Consequence:** the first pass's automated search had matched a different
  Jones paper and rejected it; this supplied PDF is the correct paper. One
  cohort.
- **Proposed status:** `evidenced`, `fulltext`.

### S006 Chiu 2024, rows M008 (dissoc_cps), M009 (dissoc_del), M010 (dissoc_hal)

- **Source:** Chiu, Li, Hsieh, Chou, Au, Chen & Lin (2023 online / 2024 print).
  *Psychological Trauma: Theory, Research, Practice, and Policy*, 16(S1),
  S242–S249.
- **Locator:** p. 3 (PDF), Method, Participants and Procedures; Table 1, Table
  3.
- **Establishes:** the paper recruits four groups (SCZ n = 43, BD n = 32, MDD
  n = 59, healthy controls n = 40); the schizophrenia subgroup is exactly n =
  43. Both the CADSS (clinician-rated) and the DSS (patient-rated) were
  administered to this SCZ subsample, and Table 3 reports SCZ-specific paths
  CADSS→PANSS-positive (dissoc_cps), DSS→hallucination scale (dissoc_hal) and
  DSS→delusion inventory (dissoc_del) — exactly the registry's three rows.
- **Corroboration:** registry n = 43, matching the SCZ subgroup exactly (not
  the paper's total N).
- **Consequence:** the first pass's automated search had matched a different
  Chiu paper and rejected it; this supplied PDF is the correct paper, and its
  four-diagnostic-group design does not create a collision because only the
  n = 43 SCZ subgroup maps to this cohort.
- **Proposed status:** `evidenced`, `fulltext`.

### S007 Sun 2018 (registry label "Sun, 2018"), rows M011 (dissoc_del), M012 (dissoc_hal)

- **Source:** Sun, Alvarez-Jimenez, Simpson, Lawrence, Peach & Bendall (2018).
  *Comprehensive Psychiatry*, 84, 68–74.
- **Locator:** p. 2 (PDF), Method, 2.1 Participants; Table 5.
- **Establishes:** "A total of 70 participants with FEP were recruited...
  final sample size was 66," clients of the EPPIC early-psychosis service in
  Melbourne. The SCID-D-R was the primary dissociation measure, matching the
  registry.
- **Corroboration:** registry n = 66.
- **Consequence:** one cohort, both pathways formally tested with the SCID-D-R
  (the hallucination path was non-significant; that is a result, not an
  evidence gap).
- **Proposed status:** `evidenced`, `fulltext`.

### S008 Perona 2012 (registry label "Perona‚ 2012"), rows M013 (dissoc_del), M014 (dissoc_hal)

- **Source:** Perona-Garcelán, Carrascoso-López, García-Montes,
  Ductor-Recuerda, López Jiménez, Vallina-Fernández, Pérez-Álvarez &
  Gómez-Gómez (2012). *Journal of Traumatic Stress*, 25, 323–329.
  doi:10.1002/jts.21693.
- **Locator:** p. 324 (PDF p. 2), Method, Participants and Procedure.
- **Establishes:** "71 patients diagnosed with psychosis... participating in
  activities at the Virgen del Rocío... and Virgen Macarena" rehabilitation
  units in Seville. The Spanish DES-II was administered, matching the
  registry.
- **Corroboration:** registry n = 71.
- **Consequence:** the first pass's automated search had returned unrelated
  papers; this is the correct 2012 Perona-Garcelán paper (a distinct 2014
  paper by the same author exists elsewhere in the literature folder and was
  not used). One cohort, both pathways formally tested.
- **Proposed status:** `evidenced`, `fulltext`.

### S009 Varese 2012 (registry label "Varese, 2012"), row M015 (dissoc_hal)

- **Source:** Varese, Barkus & Bentall (2012). *Psychological Medicine*, 42,
  1025–1036.
- **Locator:** p. 1026–1027 (PDF p. 1), Method, Participants.
- **Establishes:** "45 patients with diagnoses in the schizophrenia
  spectrum... were recruited from in-patient and out-patient services in North
  Wales," plus a separate 20-person healthy-control group not part of this
  cohort. The DES was administered, matching the registry.
- **Corroboration:** registry n = 45.
- **Consequence:** the first pass's automated search had matched Varese's
  well-known 2012 trauma-and-psychosis meta-analysis, a review with no
  recruited sample of its own; this supplied PDF is the correct primary
  empirical study.
- **Proposed status:** `evidenced`, `fulltext`.

### S010 Pearce 2017, rows M016 (dissoc_del), M017 (dissoc_hal)

- **Source:** Pearce, Simpson, Berry, Bucci, Moskowitz & Varese (2017).
  *Clinical Psychology & Psychotherapy*, 24, 1304–1312.
- **Locator:** p. 1306 (PDF p. 2), Method, 2.1 Participants.
- **Establishes:** "A total of 131 self-selected eligible participants entered
  an online survey... leaving a total sample of 112." The DES-R was
  administered, matching the registry.
- **Corroboration:** registry n = 112.
- **Consequence:** one cohort used in two parallel mediation models
  (dissociation→voices, dissociation→paranoia), matching the registry's split
  into M016/M017.
- **Proposed status:** `evidenced`, `fulltext`.

### S012 Berenbaum 2008, row M019 (dissoc_cps)

- **Source:** Berenbaum, Thompson, Milanak, Boden & Bredemeier (2008).
  *Journal of Abnormal Psychology*, 117(3), 502–519.
- **Locator:** p. 508 (PDF), Study 2: In-Person Assessment, Method,
  Participants.
- **Establishes:** "The participants were 303 adults... We intentionally
  oversampled individuals with high levels of peculiar perceptions and
  beliefs." The SCID-D was administered by interview, matching the registry.
- **Corroboration:** registry n = 303, matching Study 2 exactly.
- **Consequence:** the paper reports two studies — Study 1 (N = 1,510,
  telephone survey, no SCID-D) and Study 2 (N = 303, in-person, SCID-D). Only
  Study 2 matches the registry row. Roughly 214 of the 303 Study 2 respondents
  had also taken part in Study 1, so Study 2 is not fully independent of
  Study 1, but Study 1 is not itself a mapped cohort in this registry, so this
  does not create a collision. The citation for this row should specify
  "Berenbaum et al. 2008, Study 2" to avoid future confusion with Study 1.
- **Proposed status:** `evidenced`, `fulltext`.

### S017 Boyda 2018, row M026 (negsch_cps)

- **Source:** Boyda, McFeeters, Dhingra & Rhoden (2018). *Journal of Clinical
  Psychology*, 74, 2287–2301.
- **Locator:** p. 2289 (PDF), 2 Methods and Materials, 2.1 Participants.
- **Establishes:** "resulting in a total sample of 302 participants,"
  recruited via social media. Four of fifteen YSQ-SF early-maladaptive-schema
  subscales were used as mediators, matching the registry's YSQ-SF.
- **Corroboration:** registry n = 302.
- **Consequence:** a different paper, "Boyda & McFeeters 2015," exists
  elsewhere in the literature folder and was not used; this is confirmed as
  the 2018 paper. One cohort.
- **Proposed status:** `evidenced`, `fulltext`.

### S019 Dudley 2023, row M028 (negsch_cps)

- **Source:** Dudley et al. (2023). *Schizophrenia Bulletin Open*.
  doi:10.1093/schizbullopen/sgad030.
- **Locator:** p. 3, Methods, Participants.
- **Establishes:** "292... participants were drawn from the 487 patients
  recruited for the FOCUS trial," a clozapine-resistant-schizophrenia CBT
  trial. The BCSS measured negative-self and negative-other beliefs, tested as
  mediators between childhood trauma (CTQ) and PANSS/PSYRATS symptoms,
  matching the registry's pathway and instrument.
- **Corroboration:** registry n = 292.
- **Consequence:** the first pass flagged the title as not obviously matching
  a negative-schema study; the full text confirms it is one (the abstract and
  results explicitly test negative-belief mediation). One cohort, a subsample
  of the larger FOCUS trial who completed the CTQ.
- **Proposed status:** `evidenced`, `fulltext`.

### S021 Murphy 2015, row M030 (negsch_cps)

- **Source:** Murphy, Murphy & Shevlin (2015). *British Journal of Clinical
  Psychology*, 54, 326–344.
- **Locator:** p. 329, Methods.
- **Establishes:** "An overall sample of 785 pupils participated," recruited
  from ten post-primary schools in Northern Ireland. The PTCI was one of three
  mediators tested, matching the registry.
- **Corroboration:** registry n = 785.
- **Consequence:** one school-based adolescent cohort. Publication year 2015
  confirmed from the journal header (filename lacked a year).
- **Proposed status:** `evidenced`, `fulltext`.

### S022 Simpson 2020, row M031 (negsch_cps)

- **Source:** Simpson, Helliwell, Varese & Powell (2020). *British Journal of
  Clinical Psychology*, 59, 260–275.
- **Locator:** p. 261–262, Method, Participants.
- **Establishes:** "Seventy-eight participants... who reported experiencing
  clinical levels of psychosis were recruited using social media." The SDS-R
  was administered as the mediator, matching the registry.
- **Corroboration:** registry n = 78.
- **Consequence:** one self-selected online cohort. Note for the record: the
  SDS-R measures self-disgust, which the authors frame within the
  negative-schema/maladaptive-cognitive-structure literature, but it is a
  related, not identical, construct to the BCSS/PTCI negative-self-and-other
  beliefs used in the other negsch_cps rows — a labelling precision note, not
  an identity problem.
- **Proposed status:** `evidenced`, `fulltext`.

### S023 LoPilato 2021, row M032 (negsch_del)

- **Source:** LoPilato et al. (2021). *Early Intervention in Psychiatry*, 15,
  818–827.
- **Locator:** p. 820, Methods, 2.1 Sample.
- **Establishes:** "The sample included 531 CHR individuals," drawn from the
  multi-site North American Prodrome Longitudinal Study (NAPLS-2). The BCSS
  negative-self/negative-other subscales were the mediator, matching the
  registry, with a specific indirect effect on delusional thinking and
  suspiciousness, matching the negsch_del pathway.
- **Corroboration:** registry n = 531.
- **Consequence:** one pooled multi-site cohort under a single recruitment
  protocol, not multiple independent samples. Publication year 2021 confirmed
  from the journal citation (filename lacked a year).
- **Proposed status:** `evidenced`, `fulltext`.

### S025 Appiah-Kusi 2017 (registry label "Appiah 2017"), row M034 (negsch_del)

- **Source:** Appiah-Kusi et al. (2017). *Journal of Psychiatric Research*, 88,
  89–96.
- **Locator:** p. 91, 2. Method, 2.1. Participants; Table 1.
- **Establishes:** 30 ultra-high-risk cases recruited from the OASIS clinical
  service plus 38 age- and gender-matched healthy controls recruited by
  advertisement, total N = 68. The BCSS was administered, matching the
  registry.
- **Corroboration:** registry n = 68, matching the combined case-control total
  exactly.
- **Consequence:** one case-control study reported as a single dataset; the
  registry's single row and N already reflect the combined case+control
  analysis, so this is not a two-cohort collision. The registry label "Appiah"
  is the first author's name truncated; the paper indexes as "Appiah-Kusi."
- **Proposed status:** `evidenced`, `fulltext`.

### S026 Ashford 2012, row M035 (negsch_del)

- **Source:** Ashford, Ashcroft & Maguire (2012). *Journal of Experimental
  Psychopathology*, 3(4), 624–638.
- **Locator:** p. 627, Method, Participants; Table 1.
- **Establishes:** "The final sample consisted of 135 psychology undergraduate
  students... from the University of Southampton." The BCSS negative
  subscales were retained as mediators, matching the registry.
- **Corroboration:** registry n = 135.
- **Consequence:** the first pass found this paper not indexed in Europe PMC
  and rejected unrelated automated matches; the supplied PDF is a direct,
  unambiguous match. One cohort.
- **Proposed status:** `evidenced`, `fulltext`.

### S027 Pinto-Gouveia 2014, row M036 (negsch_del)

- **Source:** Pinto-Gouveia, Matos, Castilho & Xavier (2014, published online
  2012). *Clinical Psychology and Psychotherapy*, 21, 49–61.
- **Locator:** p. 4 (PDF), Method, Participants.
- **Establishes:** "255 subjects from general community population," recruited
  across schools and private-corporation staff. The Other As Shamer Scale
  (OAS) was administered, matching the registry.
- **Corroboration:** registry n = 255.
- **Consequence:** one community cohort. Publication year 2014 confirmed from
  the journal citation (online-first in 2012, issue year 2014; filename
  lacked a year).
- **Proposed status:** `evidenced`, `fulltext`.

### S028 Østefjells 2017, row M037 (depress_cps)

- **Source:** Østefjells et al. (2017). *Psychological Medicine*, 47,
  2323–2333.
- **Locator:** p. 2324–2325 (PDF p. 2–3), Method, Participants and
  Measurements.
- **Establishes:** "A total of 261 patients met inclusion criteria," recruited
  through the multi-centre TOP Study (NORMENT). Depression was operationalised
  via the PANSS depression/anxiety subscale (items G2, G3, G6), not a
  standalone depression inventory.
- **Corroboration:** registry n = 261.
- **Consequence:** one cohort, analytically split by diagnosis (bipolar vs.
  psychotic disorder) but from a single recruitment. The registry's blank
  instrument field for this row should be filled with "PANSS
  depression/anxiety subscale (G2, G3, G6)" as a follow-up.
- **Proposed status:** `evidenced`, `fulltext`.

### S029 Steenkamp 2022, row M038 (depress_cps)

- **Source:** Steenkamp et al. *Schizophrenia Research*, 241 (2022), 306–311.
- **Locator:** p. 307–308 (PDF), 2.1 Subjects, 3.1 Sample, Table 1.
- **Establishes:** 90 patients with non-affective psychotic disorder were
  recruited (part of the Weijers et al. 2016 project); after exclusions "the
  final sample consisted of 59 subjects." Depression was measured with a
  single experience-sampling item ("I feel down"), not a standardised scale.
- **Corroboration:** registry n = 59.
- **Consequence:** the first pass flagged a workbook year of 2022 against a
  2019-dated DOI. The article history box confirms the paper was accepted and
  available online in 2019, but the citable journal issue (running header,
  volume, page numbers) is 2022 — an online-first-versus-print-issue case, not
  a different paper. The registry's 2022 is the correct citable year. One
  cohort.
- **Proposed status:** `evidenced`, `fulltext`.

### S030 Sengutta 2019, row M039 (depress_cps)

- **Source:** Sengutta et al. *European Psychiatry*, 56 (2019), 84–90.
- **Locator:** p. 85, 2.1 Participants.
- **Establishes:** "200 inpatients... who presented to the adolescent ward at
  University Medical Centre Hamburg Eppendorf... during 2015 and 2017,"
  excluding patients with psychotic disorders. Depression was measured with
  the PHQ-9, used as a partial mediator between childhood adversity and
  psychotic-like experiences.
- **Corroboration:** registry n = 200.
- **Consequence:** one cohort, single site and recruitment window. No year
  discrepancy: online date, print issue and citation are all consistently
  2019.
- **Proposed status:** `evidenced`, `fulltext`.

### S031 Mętel 2020, row M040 (depress_cps)

- **Source:** Mętel et al. *Early Intervention in Psychiatry*, 2020;14:87–96.
- **Locator:** p. 89 (PDF), 2.1 Participants; Table 1.
- **Establishes:** "The final sample consisted of 2614 individuals," an online
  survey of young adults across three Polish cities, after excluding 70 for
  psychotic disorder or antipsychotic use from 2,684 enrolled. Depression was
  measured with a 5-item CESD-R subset, the final mediator in a serial
  trauma→cognitive-bias→resilience→depression→PLE pathway.
- **Corroboration:** registry n = 2614.
- **Consequence:** the PDF filename found in the literature folder read
  "Mętel 2019," matching the paper's acceptance date, but the printed
  citable journal-issue year on every page and in the "how to cite" box is
  2020, matching the registry. Online-first-versus-print-issue case, not a
  different paper. One cohort.
- **Proposed status:** `evidenced`, `fulltext`.

### S032 Saladino 2025, row M041 (depress_cps)

- **Source:** Saladino et al. *Psychology and Psychotherapy: Theory, Research
  and Practice*, 2025;00:1–16. doi:10.1111/papt.70017.
- **Locator:** p. 3–4 (PDF), Participants and procedures.
- **Establishes:** a three-wave longitudinal panel of Italian adolescents;
  "Sample sizes for each wave were 1894 participants at T1, 1773 at T2 and
  1663 at T3." Depression was measured with the DASS-21 depression subscale.
  Registry n = 1894 matches the T1 baseline wave.
- **Corroboration:** registry n = 1894.
- **Consequence:** the first pass had found an indexed year of 2026. The
  paper's own running header, citation box and DOI metadata all read 2025;
  the "2026" figure traces to a Wiley download-footer access timestamp, not a
  publication year. The article is still early view (volume/issue "00"), so
  final print pagination could in principle move it to a later year, but the
  paper is not a different study. One cohort.
- **Proposed status:** `evidenced`, `fulltext`.

## What this means for the manuscript

After the first pass, nine of 41 assertions were `evidenced` and one was
`disputed`; 31 were not. After the second pass (1 August 2026), all 24
remaining cohorts (31 rows) resolved to `evidenced`: **40 of 41 assertions are
now `evidenced` and one, Uyan 2022, remains `disputed`. None is
`unevidenced`.** The analytical unit is therefore now externally verified for
every retained row in the release, and the Methods, abstract, results and
discussion should be updated to say so — the identity caveat sentence changes
from qualifying the unit as "derived and not externally verified" to stating
that 40 of 41 assertions are evidenced and one is disputed and excluded. The
document verifier in `bayesian/R/12_documents.R` and the `identity_caveat()`
generator in `lib_provenance.R` need to be re-run so the printed sentence in
`BAYESIAN_METAREG_METHODS.md` and `BAYESIAN_METAREG_RESULTS.md` reflects the
new counts; see the pipeline re-run notes accompanying this pass.

No row in the second pass required a split or merge of a cohort: every
resolved cohort is a single recruited sample matching its registry N and
instrument (with the case-control combination in S025 Appiah-Kusi 2017
already reflected correctly as one combined-N row, and the two-study source
paper in S012 Berenbaum 2008 resolving to its Study 2 subsample only). Gibson
2019 remains the only case where the data could have been wrong about how many
cohorts there are, and its source establishes that the existing treatment is
correct. `data_sha` is therefore unchanged by this pass, and the audited
estimates remain valid as regression benchmarks.

Two rows carry corrections worth actioning separately from this pass (which
only wrote evidence columns, per the fix that ordered identity resolution
before instrument-priority demotion): the registry studyID "Yanasaki 2016"
(S002) should be corrected to "Yamasaki 2016", a one-letter transliteration
slip confirmed against the paper; and the blank instrument field for S028
Østefjells 2017 (M037) should be filled with "PANSS depression/anxiety
subscale (G2, G3, G6)", the depression measure the paper actually used.

## What would raise the evidenced count further

One assertion remains outstanding: S016 Uyan 2022 (M025), `disputed` because
the report describes two distinct recruited groups (100 patients, 100
controls) against a registry n = 100 that matches only the patient group.
Reinstatement would require the patient-only CTQ-to-DES correlation and a
decision about which group the cohort denotes; the row is already excluded on
independent grounds (`pending_source_query`), so no estimate is affected
either way.
