# risk-stratification-brfss

Reproducible analysis pipeline for interpretable cardiovascular disease (CVD) risk stratification using BRFSS 2022 (CDC).

This repository implements an end-to-end, transparent statistical analysis workflow to support population-level risk stratification, calibration, and interpretable evidence for health risk communication and intelligent health user interfaces.

---

## Repository structure

```
risk-stratification-brfss/
├── README.md
├── LICENSE
├── scripts/
│   ├── 01_download_and_read_brfss2022.R
│   ├── 02_build_analytic_dataset.R
│   ├── 03_table1_descriptives.R
│   ├── 04_table2_logistic_or.R
│   ├── 05_key_findings.R
│   ├── 06_tableS1_unweighted_vs_weighted.R
│   ├── 07_model_performance_brier.R
│   ├── 08_explanation_stability.R
│   ├── 09_risk_stratification_calibration.R
│   ├── 10_actionable_whatif.R
│   └── run_all.R
├── R/
│   ├── config.R
│   ├── utils_io.R
│   ├── utils_survey.R
│   └── utils_vars.R
├── data/
│   ├── raw/      # empty (BRFSS raw data not uploaded)
│   └── derived/  # empty
└── outputs/      # empty
```

---

## Core configuration and utilities (`R/`)

### `config.R`
Defines global configuration shared across the entire pipeline, including directory paths, random seeds, and common constants.  
Ensures consistent behavior across scripts and supports reproducibility.

### `utils_io.R`
Provides helper functions for data ingestion, caching, and standardized handling of intermediate data objects.  
Abstracts file access and reduces duplication across analysis scripts.

### `utils_survey.R`
Implements shared logic related to BRFSS complex survey design.  
Includes construction of survey design objects and handling of strata with a single primary sampling unit (PSU).

### `utils_vars.R`
Defines harmonization rules for BRFSS variables.  
Maps raw survey encodings into interpretable binary or numeric covariates used consistently throughout the analysis.

---

## Analysis scripts (`scripts/`)

### `01_download_and_read_brfss2022.R`
Downloads the BRFSS 2022 public-use dataset released by the CDC and reads the XPT format into R.  
Ensures that the raw data source is obtained in a reproducible and documented manner.

### `02_build_analytic_dataset.R`
Constructs the analytic cohort used in all downstream analyses.  
Defines the cardiovascular disease (CVD) outcome and harmonizes covariates such as age, sex, smoking, obesity, physical inactivity, diabetes, and related health indicators.  
Applies a transparent complete-case strategy for variables included in each model.

### `03_table1_descriptives.R`
Computes descriptive summaries of the analytic cohort.  
Summarizes demographic characteristics and prevalence of major behavioral and clinical risk factors using survey-weighted estimation when applicable.

### `04_table2_logistic_or.R`
Fits an interpretable logistic regression model for the binary CVD outcome.  
Uses survey-weighted estimation via `svyglm` when BRFSS design variables are available, and standard logistic regression otherwise.  
Effect sizes are expressed as odds ratios with corresponding confidence intervals.

### `05_key_findings.R`
Extracts and organizes the main empirical findings from the fitted regression model.  
Supports concise reporting of core results in the accompanying paper.

### `06_tableS1_unweighted_vs_weighted.R`
Performs a sensitivity analysis comparing survey-weighted and unweighted logistic regression estimates.  
Evaluates robustness of effect estimates to the inclusion of survey design weights.

### `07_model_performance_brier.R`
Evaluates model-level performance with an emphasis on calibration.  
Computes predicted risks from the fitted model and summarizes calibration using the Brier score.  

### `08_explanation_stability.R`
Assesses the stability of model-based explanations under resampling or perturbation.  
Supports user-facing explanations by examining whether the relative importance of predictors remains consistent.

### `09_risk_stratification_calibration.R`
Implements risk stratification by grouping individuals into discrete risk tiers based on predicted probabilities.  
Examines observed outcome prevalence and predicted risk within each tier to support interpretable categorization and calibration-aware risk communication.

### `10_actionable_whatif.R`
Constructs model-based “what-if” illustrations by modifying selected modifiable risk factors while holding other covariates fixed.  
These scenarios are intended for illustrative risk communication and are explicitly non-causal.

### `run_all.R`
Orchestrates the full analysis pipeline by executing the individual scripts in sequence.  
Provides a single entry point for reproducing the complete workflow.

---

## Running the pipeline

To execute the full analysis pipeline sequentially:

```bash
Rscript scripts/run_all.R
```

Individual scripts can also be run independently if required upstream objects are available.

---

## Data source

The analysis uses the 2022 Behavioral Risk Factor Surveillance System (BRFSS) public-use dataset released by the U.S. Centers for Disease Control and Prevention (CDC).  
All data used are publicly available and contain no proprietary or confidential information.

---

## Associated paper

This repository supports the empirical analysis presented in:

*Interpretable Cardiovascular Risk Stratification from BRFSS 2022 to Inform Intelligent Health Interfaces*  

The codebase provides a fully reproducible implementation of the analyses reported in the paper, including descriptive statistics, interpretable regression modeling, sensitivity analysis, calibration assessment, risk tiering, explanation stability, and actionable risk messaging illustrations.
