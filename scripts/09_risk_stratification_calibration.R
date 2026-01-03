#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(readr)
  library(survey)
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

p_hat <- predict(fit_use, newdata = analytic_complete, type = "response")
analytic_complete$p_hat <- p_hat

make_tiers <- function(p, k) {
  probs <- seq(0, 1, length.out = k + 1)
  breaks <- quantile(p, probs = probs, na.rm = TRUE)
  if (length(unique(breaks)) < length(breaks)) {
    return(NULL)
  }
  cut(p, breaks = breaks, include.lowest = TRUE, labels = FALSE)
}

summarize_tiers <- function(df, tier_var, tier_type, design_obj) {
  tiers <- sort(unique(df[[tier_var]]))
  out <- vector("list", length(tiers))
  for (i in seq_along(tiers)) {
    t <- tiers[i]
    idx <- df[[tier_var]] == t
    n_t <- sum(idx, na.rm = TRUE)
    if (!is.null(design_obj)) {
      design_t <- subset(design_obj, idx)
      mean_pred <- as.numeric(svymean(~p_hat, design_t, na.rm = TRUE))
      prev <- as.numeric(svymean(~cvd, design_t, na.rm = TRUE))
    } else {
      mean_pred <- mean(df$p_hat[idx], na.rm = TRUE)
      prev <- mean(df$cvd[idx], na.rm = TRUE)
    }
    out[[i]] <- tibble(
      tier_type = tier_type,
      tier = t,
      n = n_t,
      mean_predicted = mean_pred,
      observed_prevalence = prev
    )
  }
  bind_rows(out)
}

tier_tertile <- make_tiers(analytic_complete$p_hat, 3)
if (is.null(tier_tertile)) {
  stop("Unable to compute tertiles due to non-unique breaks.")
}

analytic_complete$tier_tertile <- tier_tertile

if (is_weighted) {
  design <- create_design(analytic_complete)
} else {
  design <- NULL
}

if (is_weighted && is.null(design)) {
  log_message("[survey] Design variables unavailable; using unweighted summaries.")
}

tertile_tbl <- summarize_tiers(analytic_complete, "tier_tertile", "tertile", design)

out_tbl <- tertile_tbl

tier_decile <- make_tiers(analytic_complete$p_hat, 10)
if (!is.null(tier_decile)) {
  analytic_complete$tier_decile <- tier_decile
  decile_tbl <- summarize_tiers(analytic_complete, "tier_decile", "decile", design)
  out_tbl <- bind_rows(out_tbl, decile_tbl)
}

write_csv(out_tbl, file.path(opt$outdir, "risk_stratification_calibration.csv"))

kable(out_tbl, format = "latex", booktabs = TRUE, digits = 4) %>%
  writeLines(con = file.path(opt$outdir, "risk_stratification_calibration.tex"))

log_message("[calibration] Wrote outputs to ", opt$outdir)
