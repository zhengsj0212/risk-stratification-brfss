has_design_vars <- function(df) {
  needed <- c("psu", "strata", "weight")
  if (!all(needed %in% names(df))) return(FALSE)
  if (all(is.na(df$psu)) || all(is.na(df$strata)) || all(is.na(df$weight))) return(FALSE)
  TRUE
}

create_design <- function(df) {
  if (!has_design_vars(df)) return(NULL)
  options(survey.lonely.psu = "adjust")
  if (exists("log_message", mode = "function")) {
    log_message("[survey] survey.lonely.psu set to 'adjust'")
  } else {
    message("[survey] survey.lonely.psu set to 'adjust'")
  }
  survey::svydesign(ids = ~psu, strata = ~strata, weights = ~weight, data = df, nest = TRUE)
}
