test_that("plenarias works", {
    skip_if_offline()
    skip_on_cran()
    out <- plenarias(10)
    expect_s3_class(out, "data.frame")
})
