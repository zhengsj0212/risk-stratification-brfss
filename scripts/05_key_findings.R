#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(survey)
  library(broom)
})

source("/Users/zhengsijie/LinearRegressionGemini/healthiui/R/config.R")
source("/Users/zhengsijie/LinearRegressionGemini/healthiui/R/utils_io.R")
source("/Users/zhengsijie/LinearRegressionGemini/healthiui/R/utils_survey.R")

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

analytic_path <- file.path(data_derived_dir, "brfss2022_analytic.rds")
model_path <- file.path(data_derived_dir, "model_fit.rds")

if (!file.exists(analytic_path)) {
  stop("Analytic dataset not found: ", analytic_path)
}
if (!file.exists(model_path)) {
  stop("Model fit not found: ", model_path)
}

analytic_complete <- readRDS(analytic_path)
fit_use <- readRDS(model_path)

design <- create_design(analytic_complete)

cvd_prev <- if (!is.null(design)) {
  100 * as.numeric(svymean(~cvd, design, na.rm = TRUE))
} else {
  100 * mean(analytic_complete$cvd, na.rm = TRUE)
}

coef_tbl <- broom::tidy(fit_use, conf.int = TRUE, conf.level = 0.95)
coef_tbl <- coef_tbl %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR = exp(estimate),
    OR_low = exp(conf.low),
    OR_high = exp(conf.high)
  )

fmt_or <- function(term, label) {
  row <- coef_tbl[coef_tbl$term == term, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  if (any(is.na(row$OR)) || any(is.na(row$OR_low)) || any(is.na(row$OR_high))) return(NULL)
  sprintf("%s OR %.2f (95%% CI %.2f–%.2f).", label, row$OR[1], row$OR_low[1], row$OR_high[1])
}

bullets <- character(0)
if (is.finite(cvd_prev)) {
  bullets <- c(bullets, sprintf("Weighted CVD prevalence in 2022 BRFSS: %.1f%%.", cvd_prev))
}

bullets <- c(bullets, fmt_or("age10", "Age (per 10 years)"))
bullets <- c(bullets, fmt_or("hypertension", "Hypertension"))
bullets <- c(bullets, fmt_or("diabetes", "Diabetes"))
bullets <- c(bullets, fmt_or("smoker", "Current smoking"))

bullets <- bullets[!is.na(bullets) & nzchar(bullets)]

writeLines(paste0("- ", bullets), con = file.path(opt$outdir, "key_findings.txt"))
log_message("[key_findings] Wrote outputs to ", opt$outdir)
