get_var <- function(df, candidates) {
  for (nm in candidates) {
    if (nm %in% names(df)) return(df[[nm]])
  }
  NULL
}
