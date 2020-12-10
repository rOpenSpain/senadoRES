test_that("boletin works", {
    boletin_csv <- boletin_csv(14, 3) # Encoding is not UTF-8
    b <-  boletin(boletin_csv)
    expect_s3_class(b, "data.frame")
})
