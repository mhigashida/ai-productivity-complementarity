# AI Productivity Pilot Study

This repository contains the analysis code, processed data, and outputs for a pilot study examining whether AI productivity effects depend on task structure and worker capability.

---

## Overview

The preregistered hypotheses tested whether AI intensity interacts with task standardization to predict performance. Exploratory analyses further examined whether digital comfort (a proxy for worker capability) conditions AI productivity effects.

The theoretical framework guiding this study is complementarity: AI productivity effects may depend on alignment between technology, task structure, and worker capability rather than operating as a universal main effect.

---

## Repository Structure

ai-productivity-pilot/

├── preregistration_updated.pdf  
│  
├── code/  
│   ├── 00_setup.R  
│   ├── 01_ingest_qc.R  
│   ├── 02_constructs.R  
│   ├── 03_confirmatory_prereg.R  
│   ├── 04_prereg_outputs.R  
│   ├── 04_robustness_pcs.R  
│   ├── fig_coef_prereg_modelB_eval.R  
│   ├── fig_capability_digital_comfort.R  
│   ├── fig_margeff_digital_comfort_highperf.R  
│   └── fig_configuration_3way_facets.R  
│  
├── data/  
│   └── processed/  
│       └── analysis_constructed.csv  
│  
└── output/  
    ├── figs/  
    └── logs/  

---

## Data

Raw survey exports are not included in this repository to protect participant confidentiality.

The analytic dataset used for modeling is provided in:

data/processed/analysis_constructed.csv

All results can be reproduced from this file using the scripts in the `code/` directory.

---

## Reproducing Results

### Confirmatory preregistered analyses

Run:

source("code/00_setup.R")  
source("code/03_confirmatory_prereg.R")  
source("code/04_prereg_outputs.R")  

### Figures

Run:

source("code/fig_coef_prereg_modelB_eval.R")  
source("code/fig_capability_digital_comfort.R")  
source("code/fig_margeff_digital_comfort_highperf.R")  
source("code/fig_configuration_3way_facets.R")  

All figures will be saved to:

output/figs/

---

## Methods Summary

Models are estimated using cumulative logit models (ordinal::clm) with:

- AI intensity index  
- Task standardization index  
- AI × Task interaction  
- Digital comfort  
- Controls (tenure, experience, hours worked)  

Exploratory models include a three-way interaction to evaluate configurational patterns consistent with complementarity theory.

---

## Notes

This is a pilot study. Confidence intervals are wide, and results should be interpreted cautiously. The goal of this project is to evaluate theoretical plausibility and inform future experimental designs.

---

## License

For academic use only.