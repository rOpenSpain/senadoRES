test_that("comisiones works", {
    skip_on_cran()
    skip_if_offline()
    out <- comisiones(12)
    expect_s3_class(out, "data.frame")
    # Test a sensible error for comisiones(13)
})
