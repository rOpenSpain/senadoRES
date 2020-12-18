test_that("leyes works", {
    skip_if_offline()
    skip_on_cran()
    l <- leyes(14)
    expect_s3_class(l, "data.frame")
    expect_equal(ncol(l), 8)
    expect_equal(colnames(l), c("titulo", "urlFichaLey", "anio", "boe",
                                "urlBoe", "bloquesTematicos", "numLey", "tipo"))
})
