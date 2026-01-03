#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(readr)
  library(survey)
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
log_message("[note] Model-based what-if illustration for UI messaging; not a causal effect.")

analytic_path <- file.path(data_derived_dir, "brfss2022_analytic.csv")
model_path <- file.path(data_derived_dir, "model_fit.rds")

if (!file.exists(analytic_path)) {
  stop("Analytic CSV not found: ", analytic_path)
}
if (!file.exists(model_path)) {
  stop("Model fit not found: ", model_path)
}

df <- read_csv(analytic_path, show_col_types = FALSE)
fit_use <- readRDS(model_path)

p0 <- predict(fit_use, newdata = df, type = "response")
valid <- is.finite(p0) & !is.na(df$cvd)

if (!any(valid)) {
  stop("No valid predictions for what-if analysis.")
}

df <- df[valid, , drop = FALSE]
p0 <- p0[valid]

cutpoints <- quantile(p0, probs = c(1/3, 2/3), na.rm = TRUE)
if (length(unique(cutpoints)) < 2) {
  stop("Unable to compute tertiles from baseline predictions.")
}

tier <- cut(
  p0,
  breaks = c(-Inf, cutpoints[1], cutpoints[2], Inf),
  labels = c("low", "medium", "high"),
  include.lowest = TRUE
)

df$tier <- tier

actionable_vars <- c("smoker", "inactive", "obese")
actionable_vars <- actionable_vars[actionable_vars %in% names(df)]
actionable_vars <- actionable_vars[vapply(actionable_vars, function(v) any(!is.na(df[[v]])), logical(1))]

if (length(actionable_vars) == 0) {
  stop("No actionable variables available in analytic dataset.")
}

weight_available <- "weight" %in% names(df) && any(!is.na(df$weight))

create_weighted_design <- function(data) {
  if (!weight_available) return(NULL)
  if (all(c("psu", "strata") %in% names(data)) && any(!is.na(data$psu)) && any(!is.na(data$strata))) {
    return(create_design(data))
  }
  options(survey.lonely.psu = "adjust")
  log_message("[survey] survey.lonely.psu set to 'adjust'")
  survey::svydesign(ids = ~1, weights = ~weight, data = data)
}

mean_by_group <- function(data, value_var, design_obj) {
  if (!is.null(design_obj)) {
    if (!value_var %in% names(design_obj$variables)) {
      stop("Variable not found in survey design: ", value_var)
    }
    overall <- as.numeric(svymean(as.formula(paste0("~", value_var)), design_obj, na.rm = TRUE))
    by_tier_raw <- survey::svyby(
      as.formula(paste0("~", value_var)),
      ~tier,
      design_obj,
      survey::svymean,
      na.rm = TRUE
    )
    by_tier <- dplyr::tibble(
      tier = as.character(by_tier_raw$tier),
      mean_val = as.numeric(by_tier_raw[[value_var]])
    )
  } else {
    overall <- mean(data[[value_var]], na.rm = TRUE)
    by_tier <- data %>%
      group_by(tier) %>%
      summarize(mean_val = mean(.data[[value_var]], na.rm = TRUE), .groups = "drop")
  }
  list(overall = overall, by_tier = by_tier)
}

out_rows <- list()

for (v in actionable_vars) {
  df_cf <- df
  df_cf[[v]] <- 0L
  p1 <- predict(fit_use, newdata = df_cf, type = "response")
  dp <- p1 - p0

  df$baseline_p <- p0
  df$counter_p <- p1
  df$delta_p <- dp

  design_use <- create_weighted_design(df)
  base_means <- mean_by_group(df, "baseline_p", design_use)
  cf_means <- mean_by_group(df, "counter_p", design_use)
  dp_means <- mean_by_group(df, "delta_p", design_use)

  out_rows[[length(out_rows) + 1]] <- tibble(
    factor = v,
    group = "overall",
    baseline_mean_risk = base_means$overall,
    counterfactual_mean_risk = cf_means$overall,
    mean_delta = dp_means$overall
  )

  tiers <- c("low", "medium", "high")
  for (t in tiers) {
    base_t <- dplyr::filter(base_means$by_tier, .data$tier == t)
    cf_t <- dplyr::filter(cf_means$by_tier, .data$tier == t)
    dp_t <- dplyr::filter(dp_means$by_tier, .data$tier == t)
    if (nrow(base_t) == 0 || nrow(cf_t) == 0 || nrow(dp_t) == 0) next
    out_rows[[length(out_rows) + 1]] <- tibble(
      factor = v,
      group = t,
      baseline_mean_risk = base_t$mean_val,
      counterfactual_mean_risk = cf_t$mean_val,
      mean_delta = dp_t$mean_val
    )
  }
}

out_tbl <- bind_rows(out_rows)
write_csv(out_tbl, file.path(opt$outdir, "whatif_actionable.csv"))

bullet_lines <- character(0)
for (v in actionable_vars) {
  overall_row <- out_tbl %>% filter(factor == v, group == "overall")
  high_row <- out_tbl %>% filter(factor == v, group == "high")
  if (nrow(overall_row) > 0) {
    bullet_lines <- c(bullet_lines, sprintf("Toggling %s off: overall mean delta %.4f.", v, overall_row$mean_delta[1]))
  }
  if (nrow(high_row) > 0) {
    bullet_lines <- c(bullet_lines, sprintf("Toggling %s off: high-risk mean delta %.4f.", v, high_row$mean_delta[1]))
  }
}

writeLines(paste0("- ", bullet_lines), con = file.path(opt$outdir, "whatif_actionable.txt"))

log_message("[whatif] Wrote outputs to ", opt$outdir)
