[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R](https://img.shields.io/badge/R-%E2%89%A5%204.2-blue.svg)](https://cran.r-project.org/)
[![metaSEM](https://img.shields.io/badge/metaSEM-%E2%89%A5%201.3-blue.svg)](https://cran.r-project.org/package=metaSEM)
[![PROSPERO](https://img.shields.io/badge/PROSPERO-CRD42024542972-orange.svg)](https://www.crd.york.ac.uk/PROSPERO/view/CRD42024542972)
[![OpenMx](https://img.shields.io/badge/OpenMx-%E2%89%A5%202.21-blue.svg)](https://openmx.ssri.psu.edu/)

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

## Ai Statement

## Reference

Cheung, M. W.-L. (2015). *Meta-analysis: A structural equation modeling approach*. Wiley.

## Citation
If you use this code or data, please cite:

<details>
<summary>BibTeX</summary>
<pre><code>@article{kumaretal2026,
  title={Exploring Psychological and Biological mediators between Childhood Adversity and Psychosis: An updated Systematic Review and Meta-Analysis},
  author={Geetanjali Kumar, Ines Lepreux, Lora Bici, Fizza Mustafa, Manuel Abella, Giulia Trotta, Monica Aas, Lucia Sideli, Filippo Varese, James H MacCabe, Ricardo Twumasi, Kelly Diederen, Andrea Mechelli, Ewan Carr, Robin Murray, Whiskey Eromona, Craig Morgan, Richard Bentall, Paolo Fusar-Poli, Rodolfo Rossi, Natalia E. Fares-Otero, Amy Hardy and Luis Alameda},
  journal={British Journal of Psychiatry},
  year={2026},
  volume={tbc},
  pages={tbc},
  doi={tbc},
  url={tbc}
}
</code></pre>
</details>
