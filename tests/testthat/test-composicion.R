test_that("grupos works", {
  skip_on_cran()
  skip_if_offline()
  g <- grupos(13)
  expect_s3_class(g, "data.frame")
  expect_equal(
      colnames(g),
      c("codigo", "nombre", "siglas", "notasWeb", "fechaConstitucion",
        "fechaBaja", "total", "totalElectos", "totalDesignados", "partidoCod",
        "partidoSiglas", "partidoNombre", "partidoUrl", "partidoTotal",
        "partidoTotalElectos", "partidoTotalDesignados")
  )
})


test_that("senadores works", {
  skip_on_cran()
  skip_if_offline()
  s <- senadores()
  expect_s3_class(s, "data.frame")
  expect_equal(colnames(s),
               c("nombre", "apellidos", "legislatura", "ultCredencial",
                 "procedTipo", "procedLiteral", "procedLugar", "grupoCod",
                 "grupoSiglas", "grupoNombre", "sex"))
})
