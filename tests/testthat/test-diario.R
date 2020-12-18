test_that("sumario works", {
    skip_if_offline()
    skip_on_cran()
    sumario_csv <- sumario_csv(14, 3)
    s <- sumario(sumario_csv)
    expect_s3_class(s, "data.frame")
    expect_equal(colnames(s),
                 c("idlegislatura", "idnumero", "fechaBol", "nomSumBol", "nomSumTBol",
                   "directorioBol", "directorioXmlBol", "directorioEpubBol", "apNumRomano",
                   "apDescripcion", "subDescripcion", "objeto", "tipoExpediente",
                   "disp", "cve", "fase"))
})
