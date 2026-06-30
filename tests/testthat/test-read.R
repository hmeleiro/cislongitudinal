test_that("cis_read includes core columns when requested", {
    dir <- local_test_cache()
    create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))

    df <- cis_read(cols = "idv")

    expect_named(df, c("estudio", "fecha", "genero", "edad", "idv"))
})

test_that("cis_read accepts tidyselect helpers", {
    dir <- local_test_cache()
    create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))

    df <- cis_read(
        fecha_min = "2023-01-01",
        cols = c(dplyr::starts_with("val_min"))
    )

    expect_named(df, c("estudio", "fecha", "genero", "edad", "val_min_1", "val_min_2"))
})

test_that("cis_read accepts character vectors through all_of", {
    dir <- local_test_cache()
    create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))
    cols <- c("idv", "recuerdo")

    df <- cis_read(cols = dplyr::all_of(cols), keep_core_cols = FALSE)

    expect_named(df, cols)
})

test_that("cis_read fails with missing columns", {
    dir <- local_test_cache()
    create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))

    expect_error(cis_read(cols = "no_existe"), "Missing: no_existe")
})

test_that("cis_read validates and applies dates", {
    dir <- local_test_cache()
    create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))

    df <- cis_read(fecha_min = "2024-01-01", fecha_max = "2024-12-31")
    expect_equal(df$estudio, "3421")

    expect_error(cis_read(fecha_min = "not-a-date"), "valid date")
    expect_error(cis_read(fecha_min = "2025-01-01", fecha_max = "2024-01-01"), "cannot be later")
})

test_that("cis_read filters studies as character codes", {
    dir <- local_test_cache()
    create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))

    df <- cis_read(estudios = c(3420, 3422), cols = "idv")
    expect_equal(df$estudio, c("3420", "3422"))
})

test_that("cis_read collect false returns a lazy query", {
    dir <- local_test_cache()
    create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))

    query <- cis_read(fecha_min = "2023-01-01", collect = FALSE)
    expect_s3_class(query, "arrow_dplyr_query")
})

test_that("schema, columns, and studies are available", {
    dir <- local_test_cache()
    create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))
    write_test_manifest(file.path(dir, "manifest.json"))

    expect_true("genero" %in% cis_cols())
    expect_true(all(c("name", "type", "description") %in% names(cis_schema())))
    studies <- cis_studies()
    expect_named(studies, c("estudio", "fecha", "anio", "mes"))
})

test_that("cis_info reports row counts and manifest dates when available", {
    dir <- local_test_cache()
    create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))
    write_test_manifest(file.path(dir, "manifest.json"))
    write_test_metadata(file.path(dir, "metadata.json"))

    info <- cis_info()
    expect_equal(info$rows, 3)
    expect_equal(info$columns, 8)
    expect_equal(info$manifest_updated_at, "2026-06-30T10:27:15+0200")
})
