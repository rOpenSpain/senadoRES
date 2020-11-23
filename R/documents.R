
#' @examples
#' document_cve <- "BOCG_D_14_110_901"
#' d <- documento(document_cve)
#' head(hyper_documento(14, d$NUMEXP))
hyper_documento <- function(legislatura, numex) {
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
    warning("On construction: will need to use databases")
    tramites <- Reduce(rbind, tramites)
}
