votaciones <- function(legislatura) {
    url <- "https://www.senado.es/web/ficopendataservlet?legis=14&tipoFich=12&tipoEx=662&numEx=000002"
    xml <- get_xml(url, "UTF-8")
}
