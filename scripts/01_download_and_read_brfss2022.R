#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(optparse)
  library(haven)
})

source("proj_root/R/config.R")
source("proj_root/R/utils_io.R")

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

zip_path <- file.path(data_raw_dir, "LLCP2022XPT.zip")
zip_url <- "https://www.cdc.gov/brfss/annual_data/2022/files/LLCP2022XPT.zip"

if (!file.exists(zip_path)) {
  log_message("[download] ", zip_url)
  download.file(zip_url, destfile = zip_path, mode = "wb", quiet = TRUE)
}

all_files <- list.files(
  data_raw_dir,
  full.names = TRUE,
  recursive = TRUE,
  all.files = FALSE,
  include.dirs = FALSE
)
bn <- basename(all_files)
xpt_files <- all_files[grepl("\\.xpt\\s*$", bn, ignore.case = TRUE)]

if (length(xpt_files) == 0) {
  unzip(zip_path, exdir = data_raw_dir)
}

all_files <- list.files(
  data_raw_dir,
  full.names = TRUE,
  recursive = TRUE,
  all.files = FALSE,
  include.dirs = FALSE
)
bn <- basename(all_files)
xpt_files <- all_files[grepl("\\.xpt\\s*$", bn, ignore.case = TRUE)]

if (length(xpt_files) == 0) {
  message("[debug] recursive files: ", paste(basename(all_files), collapse = ", "))
  dput(basename(all_files))
  for (nm in basename(all_files)) cat(nm, ":", paste(charToRaw(nm), collapse = " "), "\n")
  stop("No .xpt file found after unzipping BRFSS data")
}
if (length(xpt_files) > 1) {
  warning("Multiple .xpt files found; using the first one: ",
          paste(basename(xpt_files), collapse = ", "))
}

xpt_path <- xpt_files[1]
log_message("[data] Using XPT file: ", basename(xpt_path))

brfss <- haven::read_xpt(xpt_path)

saveRDS(brfss, file.path(data_derived_dir, "brfss2022_raw.rds"))
log_message("[data] Saved raw BRFSS RDS: ", file.path(data_derived_dir, "brfss2022_raw.rds"))
