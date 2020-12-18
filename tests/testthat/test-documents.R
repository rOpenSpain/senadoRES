test_that("iniciativa_parlamentaria works", {
    skip_if_offline()
    skip_on_cran()
    hd <- iniciativa_parlamentaria(14, "661/000073")
    expect_s3_class(hd, "data.frame")
    expect_equal(colnames(hd),
                 c("bolCamara", "bolSerie", "bolNumpag", "bolNumero",
                   "bolFecha", "bolDescripcion", "bolURL", "titulo",
                   "situacion", "tipo", "fechaPresentacion",
                   "fechaCalificacion", "legislatura", "numero",
                   "expRelacionados", "procedimiento", "observaciones",
                   "autores", "organosCompetentes",
                   "areasTematicas", "tramites", "sessiones"))
})
