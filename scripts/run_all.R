#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(optparse)
})

source("proj_root/R/config.R")
source("proj_root/R/utils_io.R")

opt_list <- list(
  make_option(c("-o", "--outdir"), type = "character", default = output_dir_default,
              help = "Output directory [default %default]"),
  make_option(c("-s", "--seed"), type = "integer", default = 123,
              help = "Random seed [default %default]")
)

opt <- parse_args(OptionParser(option_list = opt_list))
set.seed(opt$seed)

ensure_dirs(c(data_raw_dir, data_derived_dir, output_dir_default, logs_dir, opt$outdir))
log_message("[config] project_root: ", project_root)

scripts <- c(
  "01_download_and_read_brfss2022.R",
  "02_build_analytic_dataset.R",
  "03_table1_descriptives.R",
  "04_table2_logistic_or.R",
  "05_key_findings.R",
  "06_tableS1_unweighted_vs_weighted.R",
  "07_model_performance_auc_brier.R",
  "08_explanation_stability.R",
  "09_risk_stratification_calibration.R",
  "10_actionable_whatif.R"
)

for (script_name in scripts) {
  script_path <- file.path(project_root, "scripts", script_name)
  log_message("[run] ", script_name)
  status <- system2("Rscript", c(script_path, "--outdir", opt$outdir, "--seed", opt$seed))
  if (!identical(status, 0L)) {
    stop("Pipeline step failed: ", script_name)
  }
}

log_message("[run] Pipeline complete")
