test_that("cis_cache_dir returns a valid path", {
    dir <- local_test_cache()
    expect_equal(cis_cache_dir(), dir)
})

test_that("cis_available is false without a local file", {
    local_test_cache()
    expect_false(cis_available())
})

test_that("cis_path returns the local path when the file exists", {
    dir <- local_test_cache()
    create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))
    expect_match(cis_path(), "cis-longitudinal[.]parquet$")
})

test_that("cis_clear removes local files without confirmation", {
    dir <- local_test_cache()
    create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))
    write_test_manifest(file.path(dir, "manifest.json"))
    write_test_metadata(file.path(dir, "metadata.json"))

    expect_true(cis_clear(confirm = FALSE))
    expect_false(file.exists(file.path(dir, "cis-longitudinal.parquet")))
    expect_false(file.exists(file.path(dir, "manifest.json")))
    expect_false(file.exists(file.path(dir, "metadata.json")))
})

test_that("cis_clear aborts in non-interactive mode when confirmation is required", {
    dir <- local_test_cache()
    create_test_parquet(file.path(dir, "cis-longitudinal.parquet"))
    expect_error(cis_clear(confirm = TRUE), "not interactive")
})
