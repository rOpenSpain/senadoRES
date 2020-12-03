test_that("sumario works", {
    skip_if_offline()
    skip_on_cran()
    sumario_csv <- sumario_csv(legislatura = 14, sesion = 1)
    s <- sumario(sumario_csv)
    expect_s3_class(s, "data.frame")
})
