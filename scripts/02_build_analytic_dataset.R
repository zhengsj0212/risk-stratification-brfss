#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(readr)
})

source("proj_root/R/config.R")
source("proj_root/R/utils_io.R")
source("proj_root/R/utils_vars.R")
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

raw_path <- file.path(data_derived_dir, "brfss2022_raw.rds")
if (!file.exists(raw_path)) {
  stop("Raw BRFSS RDS not found: ", raw_path)
}

brfss <- readRDS(raw_path)

mic_hd <- get_var(brfss, c("_MICHD"))
cvd_strk <- get_var(brfss, c("CVDSTRK3"))

if (is.null(mic_hd) || is.null(cvd_strk)) {
  stop("Outcome variables _MICHD and/or CVDSTRK3 not found")
}

cvd <- ifelse(mic_hd == 1 | cvd_strk == 1, 1L,
              ifelse(mic_hd == 2 & cvd_strk == 2, 0L, NA_integer_))

age_raw <- get_var(brfss, c("_AGE80", "AGE80", "AGE"))
if (is.null(age_raw)) stop("Age variable not found")

age <- suppressWarnings(as.numeric(age_raw))

sex_raw <- get_var(brfss, c("SEXVAR", "_SEX"))
if (is.null(sex_raw)) stop("Sex variable not found")

female <- ifelse(sex_raw == 2, 1L, ifelse(sex_raw == 1, 0L, NA_integer_))

smoker3 <- get_var(brfss, c("SMOKER3"))
smoke100 <- get_var(brfss, c("SMOKE100"))
smokeday2 <- get_var(brfss, c("SMOKDAY2"))

smoker <- rep(NA_integer_, nrow(brfss))
if (!is.null(smoker3)) {
  smoker <- ifelse(smoker3 %in% c(1, 2), 1L,
                   ifelse(smoker3 %in% c(3, 4), 0L, NA_integer_))
} else if (!is.null(smoke100) && !is.null(smokeday2)) {
  smoker <- ifelse(smoke100 == 1 & smokeday2 %in% c(1, 2), 1L,
                   ifelse(smoke100 == 2, 0L, NA_integer_))
}

bmi_cat <- get_var(brfss, c("_BMI5CAT"))
bmi_raw <- get_var(brfss, c("_BMI5"))

obese <- rep(NA_integer_, nrow(brfss))
if (!is.null(bmi_cat)) {
  obese <- ifelse(bmi_cat == 4, 1L,
                  ifelse(bmi_cat %in% c(1, 2, 3), 0L, NA_integer_))
} else if (!is.null(bmi_raw)) {
  bmi <- suppressWarnings(as.numeric(bmi_raw)) / 100
  obese <- ifelse(is.finite(bmi), ifelse(bmi >= 30, 1L, 0L), NA_integer_)
}

paindx2 <- get_var(brfss, c("_PAINDX2"))
exerany2 <- get_var(brfss, c("EXERANY2"))

inactive <- rep(NA_integer_, nrow(brfss))
if (!is.null(paindx2)) {
  inactive <- ifelse(paindx2 == 2, 1L, ifelse(paindx2 == 1, 0L, NA_integer_))
} else if (!is.null(exerany2)) {
  inactive <- ifelse(exerany2 == 2, 1L, ifelse(exerany2 == 1, 0L, NA_integer_))
}

diab_raw <- get_var(brfss, c("DIABETE4"))
if (is.null(diab_raw)) stop("DIABETE4 not found")

diabetes <- ifelse(diab_raw == 1, 1L,
                    ifelse(diab_raw %in% c(2, 3, 4), 0L, NA_integer_))

bp_candidates <- c("BPHIGH6", "BPHIGH5", "BPHIGH4", "BPHIGH3", "BPHIGH2", "BPHIGH", "_RFHYPE6", "_RFHYPE5")
bp_raw <- get_var(brfss, bp_candidates)
used_hypertension_var <- bp_candidates[bp_candidates %in% names(brfss)][1]

if (is.null(bp_raw) || is.na(used_hypertension_var)) {
  hyp_cols <- grep("BPHIGH|HYPE|HYP", names(brfss), value = TRUE, ignore.case = TRUE)
  message("[diagnostic] hypertension-like columns: ", paste(hyp_cols, collapse = ", "))
  log_message("[warning] No hypertension variable found in BRFSS 2022; hypertension will be excluded.")
  hypertension <- rep(NA_integer_, nrow(brfss))
} else {
  log_message("[vars] hypertension variable used: ", used_hypertension_var)
  if (grepl("^BPHIGH", used_hypertension_var, ignore.case = TRUE)) {
    hypertension <- ifelse(bp_raw == 1, 1L,
                           ifelse(bp_raw == 2, 0L, NA_integer_))
  } else {
    hypertension <- ifelse(bp_raw == 1, 1L,
                           ifelse(bp_raw == 2, 0L, NA_integer_))
  }
}

psu <- get_var(brfss, c("_PSU"))
strata <- get_var(brfss, c("_STSTR"))
weight <- get_var(brfss, c("_LLCPWT"))

analytic <- tibble(
  cvd = cvd,
  age = age,
  age10 = age / 10,
  female = female,
  smoker = smoker,
  obese = obese,
  inactive = inactive,
  diabetes = diabetes,
  hypertension = hypertension,
  psu = psu,
  strata = strata,
  weight = weight
)

has_hypertension <- !all(is.na(hypertension))
vars_needed <- c("cvd", "age10", "female", "smoker", "obese", "inactive", "diabetes")
if (has_hypertension) {
  vars_needed <- c(vars_needed, "hypertension")
}
use_weighted <- !is.null(psu) && !is.null(strata) && !is.null(weight)
if (use_weighted) {
  vars_needed <- c(vars_needed, "psu", "strata", "weight")
}

analytic_complete <- analytic %>%
  filter(if_all(all_of(vars_needed), ~ !is.na(.)))

raw_n <- nrow(analytic)
analytic_n <- nrow(analytic_complete)

missing_counts <- sapply(analytic[vars_needed], function(x) sum(is.na(x)))
missing_tbl <- tibble(variable = names(missing_counts), missing = as.integer(missing_counts))

log_message("[summary] N raw: ", raw_n)
log_message("[summary] N analytic: ", analytic_n)

write_csv(analytic_complete, file.path(data_derived_dir, "brfss2022_analytic.csv"))
saveRDS(analytic_complete, file.path(data_derived_dir, "brfss2022_analytic.rds"))
write_csv(missing_tbl, file.path(logs_dir, "missingness.csv"))

var_avail <- tibble(
  variable = c("cvd", "age", "female", "smoker", "obese", "inactive", "diabetes", "hypertension", "psu", "strata", "weight"),
  available = c(
    !all(is.na(cvd)),
    !all(is.na(age)),
    !all(is.na(female)),
    !all(is.na(smoker)),
    !all(is.na(obese)),
    !all(is.na(inactive)),
    !all(is.na(diabetes)),
    has_hypertension,
    !is.null(psu),
    !is.null(strata),
    !is.null(weight)
  )
)
write_csv(var_avail, file.path(logs_dir, "variable_availability.csv"))
