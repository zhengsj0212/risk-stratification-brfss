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
}

fit_unweighted <- glm(
  model_formula,
  data = analytic_complete,
  family = binomial()
)

fit_use <- if (!is.null(fit_weighted)) fit_weighted else fit_unweighted
saveRDS(fit_use, file.path(data_derived_dir, "model_fit.rds"))

coef_tbl <- broom::tidy(fit_use, conf.int = TRUE, conf.level = 0.95)
coef_tbl <- coef_tbl %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR = exp(estimate),
    OR_low = exp(conf.low),
    OR_high = exp(conf.high)
  ) %>%
  select(term, OR, OR_low, OR_high)

write_csv(coef_tbl, file.path(opt$outdir, "table2_or.csv"))

kable(coef_tbl, format = "latex", booktabs = TRUE, digits = 3) %>%
  writeLines(con = file.path(opt$outdir, "table2.tex"))

log_message("[table2] Wrote outputs to ", opt$outdir)
