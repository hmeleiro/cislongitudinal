.cis_dataset <- "cis-longitudinal"
.cis_version <- "v1.0.0"
.cis_parquet_file <- "cis-longitudinal.parquet"
.cis_manifest_file <- "manifest.json"
.cis_metadata_file <- "metadata.json"
.cis_base_url <- "https://data.spainelectoralproject.com/cis-longitudinal/v1.0.0"
.cis_parquet_url <- paste0(.cis_base_url, "/", .cis_parquet_file)
.cis_manifest_url <- paste0(.cis_base_url, "/", .cis_manifest_file)
.cis_user_agent <- "cislongitudinal/0.1.0"
.cis_core_cols <- c("estudio", "fecha", "genero", "edad")

`%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L) y else x
}
