# risk-stratification-brfss
# Risk Stratification from BRFSS 2022

This repository contains a fully reproducible analysis pipeline for population-level
cardiovascular risk stratification using the 2022 Behavioral Risk Factor Surveillance
System (BRFSS).

## Overview

We analyze BRFSS 2022 data to identify interpretable and actionable risk factors
associated with cardiovascular disease (CVD). The analysis emphasizes transparent
modeling, survey design awareness, and outputs that support downstream risk
communication, stratification, and decision support.

## Data

BRFSS 2022 public-use data are provided by the U.S. Centers for Disease Control and
Prevention (CDC).

Due to licensing and file size constraints, raw data are **not included** in this
repository. The pipeline automatically downloads the required XPT file when running
the data ingestion script.

## Reproducibility

To run the full pipeline:

```bash
Rscript run_all.R
