test_that("cis_manifest reads local manifests", {
    dir <- local_test_cache()
    manifest <- write_test_manifest(file.path(dir, "manifest.json"))

    expect_equal(cis_manifest()$version, manifest$version)
})

test_that("cis_manifest handles invalid local JSON", {
    dir <- local_test_cache()
    writeLines("{not-json", file.path(dir, "manifest.json"))

    expect_type(cis_manifest(), "list")
    expect_length(cis_manifest(), 0)
})

test_that("cis_manifest handles unavailable remote manifests", {
    local_mocked_bindings(
        req_perform = function(req, ...) stop("offline"),
        .package = "httr2"
    )

    expect_equal(cis_manifest(remote = TRUE), list())
})

test_that("manifest parsing does not require a JSON content type", {
    text <- '{"dataset":"cis-longitudinal","version":"v1.0.0"}'
    manifest <- cislongitudinal:::cis_parse_manifest_text(text)

    expect_equal(manifest$dataset, "cis-longitudinal")
    expect_equal(manifest$version, "v1.0.0")
})

test_that("version comparison handles v-prefixed versions", {
    expect_equal(cislongitudinal:::cis_compare_versions("v1.0.1", "1.0.0"), 1L)
    expect_equal(cislongitudinal:::cis_compare_versions("v1.0.0", "1.0.0"), 0L)
    expect_equal(cislongitudinal:::cis_compare_versions("1.0.0", "v1.0.1"), -1L)
    expect_true(cislongitudinal:::cis_version_newer("v1.0.1", "1.0.0"))
})
