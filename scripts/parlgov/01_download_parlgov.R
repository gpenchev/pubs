# =============================================================================
# 01_download_parlgov.R
# Download ParlGov development dataset
# =============================================================================

source(here::here("scripts", "00_setup.R"))

parlgov_url <- "http://www.parlgov.org/data/parlgov-development_csv-utf-8.zip"

tmp <- tempfile(fileext = ".zip")

resp <- httr::GET(
  parlgov_url,
  httr::write_disk(tmp, overwrite = TRUE),
  httr::timeout(120)
)

if (httr::http_error(resp)) {
  stop("ParlGov download failed with status: ", httr::status_code(resp))
}

message("Download complete: ", round(file.size(tmp) / 1024), " KB")

tmp_dir <- file.path(tempdir(),
                     paste0("parlgov_", format(Sys.time(), "%Y%m%d%H%M%S")))
dir.create(tmp_dir, showWarnings = FALSE)

utils::unzip(tmp, exdir = tmp_dir)

csv_files <- list.files(tmp_dir, pattern = "\\.csv$", full.names = TRUE)
message("Files extracted: ", paste(basename(csv_files), collapse = ", "))

read_parlgov_table <- function(files, table_name) {
  match <- files[grepl(table_name, basename(files), fixed = TRUE)]
  if (length(match) == 0) stop("Table not found in ZIP: ", table_name)
  readr::read_csv(match[1], show_col_types = FALSE)
}

view_cabinet  <- read_parlgov_table(csv_files, "view_cabinet")
view_election <- read_parlgov_table(csv_files, "view_election")
view_party    <- read_parlgov_table(csv_files, "view_party")

message("view_cabinet rows:  ", nrow(view_cabinet))
message("view_election rows: ", nrow(view_election))
message("view_party rows:    ", nrow(view_party))

saveRDS(view_cabinet,  file.path(path_parlgov, "view_cabinet_raw.rds"))
saveRDS(view_election, file.path(path_parlgov, "view_election_raw.rds"))
saveRDS(view_party,    file.path(path_parlgov, "view_party_raw.rds"))

message("Script 01_download_parlgov complete.")
