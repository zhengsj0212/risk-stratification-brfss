#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(readr)
  library(knitr)
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

model_path <- file.path(data_derived_dir, "model_fit.rds")
analytic_path <- file.path(data_derived_dir, "brfss2022_analytic.rds")

if (!file.exists(model_path)) {
  stop("Model fit not found: ", model_path)
}
if (!file.exists(analytic_path)) {
  stop("Analytic dataset not found: ", analytic_path)
}

fit_use <- readRDS(model_path)
analytic_complete <- readRDS(analytic_path)

is_weighted <- inherits(fit_use, "svyglm")
log_message("[model] Model type: ", if (is_weighted) "weighted (svyglm)" else "unweighted (glm)")
log_message("[performance] Evaluating calibration only (Brier score); discrimination metrics omitted for survey-weighted inference.")

p_hat <- predict(fit_use, newdata = analytic_complete, type = "response")

valid <- is.finite(p_hat) & !is.na(analytic_complete$cvd)
if (!any(valid)) {
  stop("No valid predictions for performance metrics.")
}

p_hat <- p_hat[valid]
y <- analytic_complete$cvd[valid]

brier <- mean((y - p_hat) ^ 2, na.rm = TRUE)

perf_tbl <- tibble(
  Metric = "Brier score",
  Value = brier
)

write_csv(perf_tbl, file.path(opt$outdir, "model_performance.csv"))

kable(perf_tbl, format = "latex", booktabs = TRUE, digits = 4) %>%
  writeLines(con = file.path(opt$outdir, "model_performance.tex"))

log_message("[performance] Wrote outputs to ", opt$outdir)
