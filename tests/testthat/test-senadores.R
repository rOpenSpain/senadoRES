test_that("senadores works", {
    skip_if_offline()
    skip_on_cran()
    out <- senadores()
    expect_s3_class(out, "data.frame")
})
