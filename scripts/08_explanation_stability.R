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

model_path <- file.path(data_derived_dir, "model_fit.rds")
analytic_path <- file.path(data_derived_dir, "brfss2022_analytic.rds")

if (!file.exists(model_path)) {
  stop("Model fit not found: ", model_path)
}
if (!file.exists(analytic_path)) {
  stop("Analytic dataset not found: ", analytic_path)
}

fit_full <- readRDS(model_path)
analytic_complete <- readRDS(analytic_path)

model_formula <- formula(fit_full)
coef_full <- coef(fit_full)

is_weighted <- inherits(fit_full, "svyglm")
log_message("[model] Model type: ", if (is_weighted) "weighted (svyglm)" else "unweighted (glm)")

n_total <- nrow(analytic_complete)
if (n_total < 2) {
  stop("Analytic dataset too small for stability analysis.")
}

n_eval <- min(20000L, n_total)
eval_idx <- sample.int(n_total, n_eval)
log_message("[stability] Evaluation subset size: ", n_eval)

get_contrib_matrix <- function(df, formula_obj, coef_vec) {
  mm <- model.matrix(formula_obj, df)
  coef_names <- names(coef_vec)
  terms_use <- setdiff(coef_names, "(Intercept)")
  terms_use <- terms_use[terms_use %in% colnames(mm)]
  if (length(terms_use) == 0) {
    stop("No overlapping terms between model and model matrix.")
  }
  contrib <- sweep(mm[, terms_use, drop = FALSE], 2, coef_vec[terms_use], `*`)
  contrib
}

get_top_drivers <- function(contrib_mat, top_k = 3) {
  n <- nrow(contrib_mat)
  top1 <- character(n)
  topk_list <- vector("list", n)
  cols <- colnames(contrib_mat)
  for (i in seq_len(n)) {
    ord <- order(abs(contrib_mat[i, ]), decreasing = TRUE)
    k <- min(top_k, length(ord))
    top_vars <- cols[ord[seq_len(k)]]
    top1[i] <- top_vars[1]
    topk_list[[i]] <- top_vars
  }
  list(top1 = top1, topk = topk_list)
}

contrib_full <- get_contrib_matrix(analytic_complete[eval_idx, , drop = FALSE], model_formula, coef_full)
full_drivers <- get_top_drivers(contrib_full, top_k = 3)

B <- 30L
agreement_top1 <- numeric(B)
jaccard_top3 <- numeric(B)

terms_all <- colnames(contrib_full)

count_top1 <- setNames(rep(0L, length(terms_all)), terms_all)
count_top3 <- setNames(rep(0L, length(terms_all)), terms_all)

total_eval <- n_eval * B

jaccard <- function(a, b) {
  a <- unique(a)
  b <- unique(b)
  if (length(a) == 0 && length(b) == 0) return(1)
  length(intersect(a, b)) / length(union(a, b))
}

for (b in seq_len(B)) {
  train_idx <- sample.int(n_total, size = floor(0.8 * n_total), replace = FALSE)
  train_df <- analytic_complete[train_idx, , drop = FALSE]

  if (is_weighted) {
    design <- create_design(train_df)
    if (is.null(design)) {
      log_message("[stability] Design unavailable in iteration ", b, "; fitting unweighted.")
      fit_b <- glm(model_formula, data = train_df, family = binomial())
    } else {
      fit_b <- svyglm(model_formula, design = design, family = quasibinomial())
    }
  } else {
    fit_b <- glm(model_formula, data = train_df, family = binomial())
  }

  coef_b <- coef(fit_b)
  contrib_b <- get_contrib_matrix(analytic_complete[eval_idx, , drop = FALSE], model_formula, coef_b)
  drivers_b <- get_top_drivers(contrib_b, top_k = 3)

  agreement_top1[b] <- mean(drivers_b$top1 == full_drivers$top1)

  jaccard_vals <- numeric(n_eval)
  for (i in seq_len(n_eval)) {
    jaccard_vals[i] <- jaccard(drivers_b$topk[[i]], full_drivers$topk[[i]])
  }
  jaccard_top3[b] <- mean(jaccard_vals)

  for (nm in drivers_b$top1) {
    count_top1[nm] <- count_top1[nm] + 1L
  }
  for (i in seq_len(n_eval)) {
    for (nm in drivers_b$topk[[i]]) {
      count_top3[nm] <- count_top3[nm] + 1L
    }
  }
}

summary_tbl <- tibble(
  metric = c("top1_agreement", "top3_jaccard"),
  mean = c(mean(agreement_top1), mean(jaccard_top3)),
  sd = c(sd(agreement_top1), sd(jaccard_top3))
)

freq_tbl <- tibble(
  term = names(count_top1),
  top1_count = as.integer(count_top1),
  top1_pct = count_top1 / total_eval,
  top3_count = as.integer(count_top3),
  top3_pct = count_top3 / total_eval
)

out_tbl <- bind_rows(
  summary_tbl %>% mutate(section = "stability_metric") %>%
    select(section, metric, mean, sd),
  freq_tbl %>% mutate(section = "driver_frequency")
)

write_csv(out_tbl, file.path(opt$outdir, "explanation_stability.csv"))

kable(summary_tbl, format = "latex", booktabs = TRUE, digits = 4) %>%
  writeLines(con = file.path(opt$outdir, "explanation_stability.tex"))

log_message("[stability] Wrote outputs to ", opt$outdir)
