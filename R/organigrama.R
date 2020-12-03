
#' Organization chart
#'
#' Retrieves the relationships between people responsible of the Senate.
#' At the moment due to limitation on the website only works on the 13 legislature.
#' @inheritParams grupos
#' @return A `data.frame` with all the information available.
#' @export
#' @examples
#' head(organigrama())
organigrama <- function(legislatura){
    if (missing(legislatura)) {
        legislatura <- 13
    }

    if (!is_numeric(legislatura)) {
        stop("Should be numeric 0 or above")
    }
    url <- "https://www.senado.es/web/ficopendataservlet?tipoFich=5&legis="
    x <- read_xml(paste0(url, legislatura))
    s <- data.frame(
        nivel = xml_text(xml_find_all(x, "//dependencia/nivel")),
        nombreOficial = xml_text(xml_find_all(x, "//dependencia/nombreOficial")),
        nombre = xml_text(xml_find_all(x, "//dependencia/nombre")),
        codigo = xml_text(xml_find_all(x, "//dependencia/codigo")),
        codigoPadre = xml_text(xml_find_all(x, "//dependencia/codigoPadre")),
        tipoDependencia = xml_text(xml_find_all(x, "//dependencia/tipoDependencia")),
        jefe = xml_text(xml_find_all(x, "//dependencia/jefe")),
        dirCorreo = xml_text(xml_find_all(x, "//dependencia/dirCorreo")),
        numTelefono = xml_text(xml_find_all(x, "//dependencia/numTelefono"))
    )
    s
}
