
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
    out <- xml2matrix(xml_find_all(x, "./datosPersonales"))
    trimws(xml2matrix2(xml_find_all(x, paste0("./", omit_xml(c("datosPersonales", "legislaturas"))))))
    legis <- xml_find_all(x, "./legislaturas/legislatura")
    l <- lapply(legis, tidy_legislatura)
    if (length(l) == 1) {
        l[[1]]
    } else {
        Reduce(merger, l)
    }
}


tidy_legislatura <- function(x) {
    out <- xml2matrix2(xml_find_all(x, "./numLeg|numLegRomano|legislaturaActual"))
    credenciales <- xml2matrix(xml_find_all(x, "./credenciales/credencial"))
    grupoParlamentario <- xml2matrix(xml_find_all(x, "./gruposParlamentarios/grupoParlamentario"))
    mandatos <- xml2matrix(xml_find_all(x, "./mandatos/mandato"))
    cargos <- lapply(xml_find_all(x, "./cargos/cargo"), xml2matrix)
    cargos <- do.call(rbind, cargos)
    gruposTerritoriales <- NULL
    ponencias <- lapply(xml_find_all(x, "./ponencias/ponencia"), xml2matrix)
    ponencias <- do.call(rbind, ponencias)
    iniciativas <- tidy_iniciativas(xml_find_all(x, "./iniciativas/iniciativa"))
    intervenciones <- t(xml2matrix2(xml_find_all(x, "./intervenciones/intervencionTipo")))
    colnames(intervenciones) <- unique(rownames(intervenciones))
    out <- as.data.frame(out)
    our2 <- cbind(out, credenciales, grupoParlamentario, mandatos)
    df <- data.frame(
          cargos = I(list(as.data.frame(cargos))),
          ponencias = I(list(as.data.frame(ponencias))),
          iniciativas = I(list(as.data.frame(iniciativas))),
          intervenciones = I(list(as.data.frame(intervenciones))))
    cbind(our2, df)
}
