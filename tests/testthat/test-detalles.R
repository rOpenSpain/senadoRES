test_that("detalles works", {
    skip_if_offline()
    skip_on_cran()
    url <-  "https://www.senado.es/web/ficopendataservlet?tipoFich=11&legis=14&org=S000040&numSes=020&numConv=01&fecha=17112020"
    out <- detalles(url)
    expect_s3_class(out, "data.frame")
})
