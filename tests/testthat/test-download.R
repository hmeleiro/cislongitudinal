test_that("cis_download installs a validated local Parquet and metadata", {
    dir <- local_test_cache()
    source <- tempfile(fileext = ".parquet")
    create_test_parquet(source)
    manifest <- test_manifest(parquet_path = source)
    manifest$size_bytes <- unname(file.info(source)$size)
    manifest$sha256 <- digest::digest(file = source, algo = "sha256", serialize = FALSE)

    downloads <- 0L
    local_mocked_bindings(
        cis_manifest_remote = function(quiet = FALSE) manifest,
        cis_download_file = function(url, path, quiet = FALSE) {
            downloads <<- downloads + 1L
            file.copy(source, path, overwrite = TRUE)
            invisible(path)
        }
    )

    installed <- cis_download()
    expect_true(file.exists(installed))
    expect_true(file.exists(file.path(dir, "manifest.json")))
    expect_true(file.exists(file.path(dir, "metadata.json")))
    expect_equal(downloads, 1L)

    cis_download()
    expect_equal(downloads, 1L)

    cis_download(force = TRUE)
    expect_equal(downloads, 2L)
})

test_that("cis_update uses local copy when remote manifest is unavailable", {
    dir <- local_test_cache()
    create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))
    write_test_manifest(file.path(dir, "manifest.json"), version = "v1.0.0")

    local_mocked_bindings(
        cis_manifest_remote = function(quiet = FALSE) list()
    )

    expect_invisible(cis_update())
})

test_that("cis_update saves a missing local manifest when the parquet matches", {
    dir <- local_test_cache()
    local_path <- create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))
    manifest <- test_manifest(version = "v1.0.0")
    manifest$size_bytes <- unname(file.info(local_path)$size)
    manifest$sha256 <- digest::digest(file = local_path, algo = "sha256", serialize = FALSE)

    downloads <- 0L
    local_mocked_bindings(
        cis_manifest_remote = function(quiet = FALSE) manifest,
        cis_download_file = function(url, path, quiet = FALSE) {
            downloads <<- downloads + 1L
            invisible(path)
        }
    )

    cis_update()
    expect_equal(downloads, 0L)
    expect_equal(cis_manifest()$version, "v1.0.0")
})

test_that("cis_update replaces an incomplete local manifest when the parquet matches", {
    dir <- local_test_cache()
    local_path <- create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))
    jsonlite::write_json(list(parquet_url = "https://example.com/old.parquet"), file.path(dir, "manifest.json"))
    manifest <- test_manifest(version = "v1.0.0")
    manifest$size_bytes <- unname(file.info(local_path)$size)
    manifest$sha256 <- digest::digest(file = local_path, algo = "sha256", serialize = FALSE)

    downloads <- 0L
    local_mocked_bindings(
        cis_manifest_remote = function(quiet = FALSE) manifest,
        cis_download_file = function(url, path, quiet = FALSE) {
            downloads <<- downloads + 1L
            invisible(path)
        }
    )

    cis_update()
    expect_equal(downloads, 0L)
    expect_equal(cis_manifest()$version, "v1.0.0")
})

test_that("cis_update downloads when the remote version is newer", {
    dir <- local_test_cache()
    local_path <- create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))
    write_test_manifest(file.path(dir, "manifest.json"), version = "v1.0.0")

    source <- tempfile(fileext = ".parquet")
    create_test_parquet(source)
    manifest <- test_manifest(version = "v1.0.1", parquet_path = source)
    manifest$size_bytes <- unname(file.info(source)$size)
    manifest$sha256 <- digest::digest(file = source, algo = "sha256", serialize = FALSE)

    downloads <- 0L
    local_mocked_bindings(
        cis_manifest_remote = function(quiet = FALSE) manifest,
        cis_download_file = function(url, path, quiet = FALSE) {
            downloads <<- downloads + 1L
            file.copy(source, path, overwrite = TRUE)
            invisible(path)
        }
    )

    expect_true(file.exists(local_path))
    cis_update()
    expect_equal(downloads, 1L)
    expect_equal(cis_manifest()$version, "v1.0.1")
})

test_that("invalid downloaded files are rejected", {
    local_test_cache()
    manifest <- list(size_bytes = 10, sha256 = paste(rep("0", 64), collapse = ""))
    path <- tempfile()
    writeBin(charToRaw("bad"), path)

    expect_error(cislongitudinal:::cis_validate_file(path, manifest), "size")
})
