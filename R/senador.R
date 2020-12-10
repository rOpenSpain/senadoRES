
#' Retrieves the open data of a senator.
#'
#'
#' @inheritParams sumario_csv
#' @param codigo Number or idweb of the senator.
#' @examples
#' if (interactive()) {
#'     s <- senador(14, 19050)
#' }
senador <- function(legislatura, codigo) {
    url <- compose_url(tipoFich = 1, cod = codigo, legis = legislatura)
    xml <- get_xml(url, "UTF-8")
    tidy_senador(xml)
}


tidy_senador <- function(x) {
    out <- xml2matrix(xml_find_all(xml, "./datosPersonales"))
    trimws(xml2matrix2(xml_find_all(xml, paste0("./", omit_xml(c("datosPersonales", "legislaturas"))))))
    legis <- xml_find_all(xml, "./legislaturas/legislatura")
    l <- lapply(legis, tidy_legislatura)
}


tidy_legislatura <- function(x) {
    out <- xml2matrix2(xml_find_all(x, "./numLeg|numLegRomano|legislaturaActual"))
    credenciales <- xml2matrix(xml_find_all(x, "./credenciales/credencial"))
    browser()
}
