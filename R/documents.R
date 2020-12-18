
#' Individual document of *Iniciativa parlamentaria*
#'
#' Retrieve information about an individual *iniciativa parlamentaria*.
#' @inheritParams sumario_csv
#' @param numex String code of the document of the form XXX/YYYYYY.
#' @return A data.frame with the situation, the title the type of document, dates
#' and many more.
#' @examples
#' document_csv <- "BOCG_D_14_110_901"
#' if (interactive()){
#'     d <- documento(document_csv)
#'     hd <- iniciativa_parlamentaria(14, d$NUMEXP)
#' }
iniciativa_parlamentaria <- function(legislatura, numex) {
    if (!is_numeric(legislatura) && !length(legislatura) == 1) {
        stop("Legislatura should be a numeric value", call. = FALSE)
    }

    if (length(numex) != 1 && !is.character(numex)) {
        stop("numex should be like '661/000073'", call. = FALSE)
    }

    ids <- strsplit(numex, fixed = TRUE, "/")[[1]]
    url <- paste0("https://www.senado.es/web/ficopendataservlet?legis=",
                  legislatura, "&tipoFich=3&tipoEx=", ids[1],
                  "&numEx=", ids[2])
    xml <- read_xml(url)
    xml2matrix2(xml_find_all(xml, "//tramitacion"))
    tramites <- xml_find_all(xml, "/fichaExpediente/tramitaciones/tramitacion")
    tramites <- lapply(tramites, xml2matrix)
    tramites <- Reduce(rbind, tramites)
    sessiones <- xml2matrix(xml_find_all(xml, ".//sesion"))
    intervenciones <- lapply(xml_find_all(xml, ".//intervenciones/intervencion"), xml2matrix)
    intervenciones <- Reduce(rbind, intervenciones)
    publicaciones <- xml2matrix(xml_children(xml_find_all(xml, ".//boletines")))
    cabecera <- xml2matrix2(xml_children(xml_find_all(xml, ".//datosCabecera")))
    out <- as.data.frame(cabecera)
    out$autores <- list(xml2matrix(xml_children(xml_find_all(xml, ".//datosCabecera/autores"))))
    out$organosCompetentes <- list(xml_text(xml_children(xml_find_all(xml, ".//datosCabecera/organosCompetentes"))))
    out$areasTematicas <- list(xml_text(xml_children(xml_find_all(xml, ".//datosCabecera/areasTematicas"))))
    out$tramites <- list(tramites)
    out$sessiones <- list(sessiones)
    cbind.data.frame(publicaciones, out)
}
