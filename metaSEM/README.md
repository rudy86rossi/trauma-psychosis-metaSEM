# MetaSEM: Childhood Trauma, Mediators, and Psychosis

This repository contains data and R code for a meta-analytic structural equation modelling (metaSEM) study examining the indirect pathways from childhood trauma to psychotic symptoms through three mediators.

## Model

**Childhood Trauma → Mediator → Psychotic Symptoms**

Three parallel mediation analyses:
- **Dissociation** → Hallucinations, Delusions, Composite Psychosis Score
- **Depression** → Composite Psychosis Score
- **Negative Self-Schemas** → Composite Psychosis Score, Delusions

## Methods

Two-stage structural equation modelling (TSSEM) using the `metaSEM` package in R (Cheung, 2015). Random-effects models were used unless otherwise noted. Each analysis includes stage 1 (pooled correlation matrices), stage 2 (path estimation), leave-one-out sensitivity analyses, and meta-regression (age, sex, study quality).

## Repository Structure

```
metaSEM/
├── dissociation/       # CT → Dissociation → Psychosis
├── depression/         # CT → Depression → Psychosis
├── negative_schemata/  # CT → Negative Self-Schemas → Psychosis
└── References/         # Key references and supplementary materials
```

Each analysis folder contains:
- `*_mastercopy.xlsx` — original raw data as received
- `*_workcopy.xlsx` — cleaned working dataset
- `*.R` — analysis script
- `*.pdf` — results output

## Software

- R with packages: `metaSEM`, `OpenMx`, `readxl`, `ggplot2`, `tidyverse`, `semPlot`, `flextable`, `gridExtra`

## Reference

Cheung, M. W.-L. (2015). *Meta-analysis: A structural equation modeling approach*. Wiley.
