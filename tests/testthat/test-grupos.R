test_that("grupos works", {
    skip_if_offline()
    skip_on_cran()
    g <- grupos(12)
    expect_s3_class(g, "data.frame")
})
