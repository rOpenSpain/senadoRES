
#' Organization chart
#'
#' Retrieves the relationships between people responsible
#' @return A `data.frame` with all the information available.
#' @export
#' @examples
#' head(organigrama())
organigrama <- function(){
    x <- read_xml("https://www.senado.es/web/ficopendataservlet?tipoFich=5")
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
