#' *Mociones*
#'
#' Download
#' @inheritParams iniciativas
#' @return A data.frame with information about the *mociones*
#' @seealso iniciativas_parlamentarias()
#' @examples
#' if (interactive() {
#'     mociones(13)
#' }
mociones <- function(legislatura) {
    url <- "https://www.senado.es/web/ficopendataservlet?tipoFich=16&legis="
    url <- paste0(url, legislatura)
    xml <- get_xml(url, encoding = "UTF-8")
    iniciativas <- xml_find_all(xml, "./iniciativa")
    out <- lapply(iniciativas, tidy_iniciativas)
    Reduce(merger, out)
}


