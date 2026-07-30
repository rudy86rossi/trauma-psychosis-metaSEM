# Source audit and independent-sample decisions

**Date:** 29 July 2026

**Scope:** Evans et al. (2015), Uyan et al. (2022), and Blose et al. (2023)

## Analytical-unit rule

The analytical unit is one independent participant sample, not one article, spreadsheet row, instrument, exposure subtype, or reported model. A `sample_id` must identify a unique set of participants recruited into a particular analytical cohort. All effect estimates calculated from those participants share that `sample_id`.

For the primary three-correlation mediation analysis, one independent sample may contribute no more than one \(r_{XM}, r_{MY}, r_{XY}\) triplet to a given pathway. Multiple eligible measures from the same participants do not increase \(k\). They must be handled by a prespecified instrument hierarchy, by replacing the primary measure in a sensitivity analysis, or by a genuinely multivariate model containing their full within-sample sampling covariance. Correlations must not be averaged merely to reduce multiple rows to one.

The primary selection hierarchy is:

1. match the prespecified mediator and outcome constructs;
2. prefer a measure commensurate with those used by the other included samples;
3. prefer an unadjusted zero-order correlation from the intended analytical sample;
4. prefer a total score over a subscale when the pathway is defined at the composite level; and
5. use the alternative eligible measure as a named sensitivity analysis.

If a publication reports several trauma subscales from the same participants, these also share one `sample_id`. They may represent separate prespecified pathways, but they cannot be treated as independent samples within the same pooled model.

## Summary decisions

| Publication | `sample_id` | Independent sample | Primary-analysis decision |
|---|---|---|---|
| Evans et al. (2015) | `Evans2015_pooled_case_control` | 29 early-intervention psychosis participants and 31 non-clinical participants, pooled \(N=60\) | Exclude from the correlation-based MASEM because the extracted entries are unstandardised regression coefficients for binary group membership, not zero-order correlations |
| Uyan et al. (2022) | `Uyan2022_schizophrenia` | 100 patients with schizophrenia | Hold out pending recovery of the missing patient-sample CTQ total to DES correlation; do not use coefficients from the separate \(N=200\) case-control mediation |
| Blose et al. (2023) | `Blose2023_undergraduates` | Final analytical sample of 346 undergraduates | Use DES-II as the primary mediator for construct commensurability and PDEQ as a sensitivity replacement; never average the two triplets |

## Evans et al. (2015)

### Source

[Evans et al. (2015), *Trauma and psychosis: The mediating role of self-concept clarity and dissociation*](../literature/Evans%20et%20al%202015.pdf)

### Sample and outcome

The article reports 29 participants recruited from five Early Intervention in Psychosis teams and 31 non-clinical adult learners. The mediation analyses pool these groups, giving one analytical sample of \(N=60\). The dependent variable in Table 3 is binary clinical group membership. PANSS was used in the clinical services and to describe eligibility for the psychosis group; it was not the continuous outcome of the pooled mediation model and was unavailable for the non-clinical group.

The six Evans rows in `data/290726 from rodolfo/metanalisis mediation dissociation workcopy.xlsx`, sheet `Extraction`, rows 32 to 37, therefore represent six models from the same participants, not six independent samples.

### Why the extracted values are invalid as correlations

Table 3 on PDF page 4, article page 629, explicitly labels all displayed values as unstandardised regression coefficients. For CTQ total, the table reports \(a=0.013\), \(b=2.040\), and \(c'=0.049\). The remaining rows report \(b\) coefficients between 2.092 and 2.592. These values were entered in the workbook columns labelled `r_XM`, `r_MY`, and `r_XY`, which explains the impossible correlations greater than one.

The article states that CTQ measures and DES-II were significantly correlated, but it does not report the complete numerical zero-order correlation matrix needed for the proposed correlation-based MASEM. Its reported \(r=-0.47\) is an effect-size summary of the clinical versus non-clinical DES-II difference, not the required mediator-to-continuous-psychosis correlation.

### Decision

Evans et al. should be excluded from the primary correlation-based MASEM with the reason:

```text
Incommensurate effect type: the publication reports unstandardised
mediation coefficients for binary psychosis-group membership and does
not provide the required zero-order CTQ, DES-II, and continuous
psychosis-outcome correlation triplet.
```

It may be reconsidered only if participant-level data or a complete commensurate correlation matrix can be obtained. A separate synthesis of mediation with binary case status would answer a different question and should not be combined with symptom-severity correlations.

## Uyan et al. (2022)

### Source

[Uyan et al. (2022), *Relationships between childhood trauma and dissociative, psychotic symptoms in patients with schizophrenia: a case-control study*](../literature/Uyan%20-%202022%20-%20Relationships%20between%20childhood%20trauma%20and%20dissociative%20%20psychotic%20symptoms%20in%20patients.pdf)

### Two distinct analytical samples and estimands

The publication recruited 100 patients with schizophrenia and 100 healthy controls. The psychosis symptom correlations in Table 3 are calculated only in the 100 patients. This patient cohort is the relevant independent sample for a mediation model in which \(Y\) is PANSS total symptom severity.

The separate Figure 2 mediation instead uses presence of schizophrenia as a binary dependent variable and therefore concerns the combined case-control design. Its displayed values \(a=0.4543\), \(b=0.8720\), and \(c=0.8400\) are regression coefficients. They are not Pearson correlations and they do not belong to the patient-only PANSS correlation matrix.

### Available and missing correlations

Table 3 on PDF page 4 reports, for the schizophrenia sample:

| Required relationship | Published value |
|---|---:|
| DES with PANSS total, \(r_{MY}\) | 0.106 |
| CTQ total with PANSS total, \(r_{XY}\) | 0.193 |
| CTQ total with DES, \(r_{XM}\) | Not numerically reported |

The text states that the three variables were examined with two-tailed Pearson correlations, but the table does not display the CTQ total to DES coefficient. Figure 2 cannot be used to fill that gap because it has a different outcome, analysis sample, and effect scale.

The current row in `data/290726 from rodolfo/metanalisis mediation dissociation workcopy.xlsx`, sheet `Extraction`, row 52, contains `0.454`, `0.872`, and `1.23`. This is not a valid correlation triplet. In addition, the patient mean age is recoverable from Table 1 as 42.38 years with SD 15.38, and the patient sample contains 51 women.

### Decision

Assign `sample_id = Uyan2022_schizophrenia`, \(N=100\), and retain the record as `pending_source_query`, not as an included complete case. Request the exact patient-only Pearson correlation between CTQ total and DES. If it is obtained, use it together with \(r_{MY}=0.106\) and \(r_{XY}=0.193\). Until then, exclude Uyan from models requiring a complete three-correlation matrix.

The \(N=200\) case-control mediation coefficients must not be substituted, transformed as though they were correlations, or combined with the patient-only coefficients.

## Blose et al. (2023)

### Source

[Blose et al. (2023), *The indirect effect of peritraumatic dissociation on the relationship between childhood maltreatment and schizotypy*](../literature/Blose%20et%20al%20-%202022%20The-indirect-effect-of-peritraumatic-dissociation-on-the-relationship-between-childhood-maltreatment.pdf)

### Sample

The study initially recruited 372 undergraduates. After excluding incomplete or inattentive responses and ten participants older than 24, the final analytical sample was \(N=346\). The CTQ-SF, PDEQ, DES-II, and SPQ were administered to the same participants. The two Blose rows in the extraction workbook, rows 29 and 30, are therefore two mediator operationalisations from one independent sample.

### Correlations

Table 2 on PDF page 10, article page 5291, reports the complete four-variable correlation matrix:

| Pair | \(r\) |
|---|---:|
| CTQ-SF with PDEQ | 0.64 |
| CTQ-SF with DES-II | 0.45 |
| CTQ-SF with SPQ | 0.38 |
| PDEQ with DES-II | 0.61 |
| PDEQ with SPQ | 0.51 |
| DES-II with SPQ | 0.66 |

### Primary and sensitivity treatment

For a pathway defined as trait or general dissociation, DES-II is the more commensurate mediator because it is the trait-dissociation instrument used by several other included samples. The primary triplet should therefore be:

```text
sample_id = Blose2023_undergraduates
mediator = DES-II
N = 346
r_XM = 0.45
r_MY = 0.66
r_XY = 0.38
analysis_role = primary
```

The prespecified sensitivity analysis should replace, rather than supplement or average with, the primary triplet:

```text
sample_id = Blose2023_undergraduates
mediator = PDEQ
N = 346
r_XM = 0.64
r_MY = 0.51
r_XY = 0.38
analysis_role = sensitivity_alternative_mediator
```

PDEQ measures dissociation during the traumatic event, whereas DES-II measures trait dissociative experiences in daily life. Their distinction is substantively meaningful, and their correlation is 0.61 rather than unity. A mechanical average would create an instrument with no direct construct definition, understate the difference between mediator types, and incorrectly conceal the dependence induced by shared participants, trauma exposure, and outcome.

If a future model includes both mediators simultaneously, it must use the full four-variable correlation matrix and the corresponding within-sample sampling covariance. Even in that model, Blose remains one independent sample and contributes one unit to \(k\).

## Consequences for study counts and meta-regression

Study counts must be recomputed after applying these decisions and after resolving all other repeated-sample rows. `nrow(data)` is not an acceptable value for \(k\); the relevant count is the number of distinct included `sample_id` values.

Moderator values belong to the sample and must appear only once per independent sample in the primary analysis. Blose must not receive double weight because it has two mediator instruments. Evans must not receive sixfold weight because it reports CTQ total and five subscales. Uyan must not combine coefficients from the \(N=200\) case-control analysis with correlations from the \(N=100\) schizophrenia sample.

All feasibility thresholds and moderator contrasts in the Bayesian plan must be recalculated from the locked set of distinct included samples.
