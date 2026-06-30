local_test_cache <- function() {
    dir <- tempfile("cislongitudinal-cache-")
    dir.create(dir, recursive = TRUE)
    withr::local_options(
        list(cislongitudinal.cache_dir = dir),
        .local_envir = parent.frame()
    )
    dir
}

create_test_parquet <- function(path = file.path(local_test_cache(), "cis-longitudinal.parquet")) {
    df <- tibble::tibble(
        estudio = c("3420", "3421", "3422"),
        fecha = as.Date(c("2023-01-01", "2024-06-15", "2025-02-20")),
        genero = c("Mujer", "Hombre", "Mujer"),
        edad = c(35L, 47L, 52L),
        idv = c("PSOE", "PP", "SUMAR"),
        recuerdo = c("PSOE", "PP", "UP"),
        val_min_1 = c(4.5, 5.2, 6.1),
        val_min_2 = c(3.8, 4.1, 5.0)
    )
    arrow::write_parquet(df, path)
    path
}

write_test_manifest <- function(path = cislongitudinal:::cis_manifest_path(), version = "v1.0.0") {
    manifest <- test_manifest(version = version)
    jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE)
    manifest
}

write_test_metadata <- function(path = cislongitudinal:::cis_metadata_path()) {
    metadata <- list(downloaded_at = "2026-06-30T10:27:15+0200")
    jsonlite::write_json(metadata, path, auto_unbox = TRUE, pretty = TRUE)
    metadata
}

test_manifest <- function(version = "v1.0.0", parquet_path = NULL) {
    schema <- lapply(
        c("estudio", "fecha", "genero", "edad", "idv", "recuerdo"),
        function(x) list(name = x, type = "string")
    )
    list(
        dataset = "cis-longitudinal",
        version = version,
        updated_at = "2026-06-30T10:27:15+0200",
        parquet_url = parquet_path %||% "https://example.com/cis-longitudinal.parquet",
        schema = schema
    )
}

`%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L) y else x
}
