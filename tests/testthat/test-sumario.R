test_that("sumario works", {
    skip_if_offline()
    skip_on_cran()
    sumario_cve <- sumario_cve(legislatura = 14, sesion = 1)
    s <- sumario(sumario_cve)
    expect_s3_class(s, "data.frame")
})
