#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(readr)
  library(survey)
  library(broom)
  library(knitr)
})

source("proj_root/R/config.R")
source("proj_root/R/utils_io.R")
source("proj_root/R/utils_survey.R")

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

pred_vars <- c("age10", "female", "smoker", "obese", "inactive", "diabetes", "hypertension")
pred_vars <- pred_vars[pred_vars %in% names(analytic_complete)]
pred_vars <- pred_vars[vapply(pred_vars, function(v) is_available(analytic_complete, v), logical(1))]
if (length(pred_vars) == 0) {
  stop("No predictors available for model fitting.")
}
model_formula <- as.formula(paste("cvd ~", paste(pred_vars, collapse = " + ")))

fit_weighted <- NULL
if (!is.null(design)) {
  fit_weighted <- svyglm(
    model_formula,
    design = design,
    family = quasibinomial()
  )
} else {
  log_message("[survey] Design variables unavailable; fitting unweighted only.")
}

fit_unweighted <- glm(
  model_formula,
  data = analytic_complete,
  family = binomial()
)

tidy_or <- function(fit_obj) {
  broom::tidy(fit_obj, conf.int = TRUE, conf.level = 0.95) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      OR = exp(estimate),
      OR_low = exp(conf.low),
      OR_high = exp(conf.high)
    ) %>%
    select(term, OR, OR_low, OR_high)
}

unweighted_tbl <- tidy_or(fit_unweighted)
weighted_tbl <- if (!is.null(fit_weighted)) tidy_or(fit_weighted) else NULL

if (!is.null(weighted_tbl)) {
  common_terms <- intersect(weighted_tbl$term, unweighted_tbl$term)
  weighted_tbl <- weighted_tbl %>% filter(term %in% common_terms)
  unweighted_tbl <- unweighted_tbl %>% filter(term %in% common_terms)
}

format_ci <- function(low, high) {
  sprintf("%.2f–%.2f", low, high)
}

if (!is.null(weighted_tbl)) {
  out_tbl <- weighted_tbl %>%
    select(term, OR_weighted = OR, CI_weighted = OR_low) %>%
    mutate(CI_weighted = format_ci(weighted_tbl$OR_low, weighted_tbl$OR_high)) %>%
    left_join(
      unweighted_tbl %>%
        select(term, OR_unweighted = OR, CI_unweighted = OR_low) %>%
        mutate(CI_unweighted = format_ci(unweighted_tbl$OR_low, unweighted_tbl$OR_high)),
      by = "term"
    )
} else {
  out_tbl <- unweighted_tbl %>%
    transmute(
      term = term,
      OR_weighted = NA_real_,
      CI_weighted = NA_character_,
      OR_unweighted = OR,
      CI_unweighted = format_ci(OR_low, OR_high)
    )
}

write_csv(out_tbl, file.path(opt$outdir, "tableS1_or_weighted_vs_unweighted.csv"))

kable(out_tbl, format = "latex", booktabs = TRUE, digits = 3) %>%
  writeLines(con = file.path(opt$outdir, "tableS1.tex"))

log_message("[tableS1] Wrote outputs to ", opt$outdir)
