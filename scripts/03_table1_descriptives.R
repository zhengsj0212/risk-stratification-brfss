#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(readr)
  library(survey)
  library(knitr)
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
if (!file.exists(analytic_path)) {
  stop("Analytic dataset not found: ", analytic_path)
}

analytic_complete <- readRDS(analytic_path)

design <- create_design(analytic_complete)

is_available <- function(df, var) {
  var %in% names(df) && any(!is.na(df[[var]]))
}

calc_table1 <- function(design_obj, df) {
  if (!is.null(design_obj)) {
    age_mean <- as.numeric(svymean(~age, design_obj, na.rm = TRUE))
    age_sd <- sqrt(as.numeric(svyvar(~age, design_obj, na.rm = TRUE)))
    pct <- function(var) 100 * as.numeric(svymean(as.formula(paste0("~", var)), design_obj, na.rm = TRUE))
  } else {
    age_mean <- mean(df$age, na.rm = TRUE)
    age_sd <- sd(df$age, na.rm = TRUE)
    pct <- function(var) 100 * mean(df[[var]], na.rm = TRUE)
  }

  out <- list(
    N = nrow(df),
    age_mean_sd = sprintf("%.1f (%.1f)", age_mean, age_sd)
  )
  if (is_available(df, "female")) out$pct_female <- sprintf("%.1f", pct("female"))
  if (is_available(df, "smoker")) out$pct_smoker <- sprintf("%.1f", pct("smoker"))
  if (is_available(df, "obese")) out$pct_obese <- sprintf("%.1f", pct("obese"))
  if (is_available(df, "inactive")) out$pct_inactive <- sprintf("%.1f", pct("inactive"))
  if (is_available(df, "diabetes")) out$pct_diabetes <- sprintf("%.1f", pct("diabetes"))
  if (is_available(df, "hypertension")) out$pct_hypertension <- sprintf("%.1f", pct("hypertension"))

  as_tibble(out)
}

table1 <- calc_table1(design, analytic_complete)
write_csv(table1, file.path(opt$outdir, "table1_participant_characteristics.csv"))

kable(table1, format = "latex", booktabs = TRUE) %>%
  writeLines(con = file.path(opt$outdir, "table1.tex"))

log_message("[table1] Wrote outputs to ", opt$outdir)
