

iniciativas <- function(legislatura) {
    if (!is_numeric(legislatura)) {
        stop("Specify a single numeric number")
    }
    if (legislatura < 5) {
        stop("Data not availabe us a higher number.")
    }
    url <- "https://www.senado.es/web/ficopendataservlet?tipoFich=9&legis="

    xml <- get_xml(paste0(url, legislatura))
    iniciativas <- xml_find_all(xml, "./iniciativa")
    out <- lapply(iniciativas, tidy_iniciativas)
}

tidy_iniciativas <- function(x){
    out <- xml2matrix(xml_find_all(x,
                            paste0("./", omit_xml(c("fichero", "listaEnmiendas")))))
    fichero <- xml2matrix(xml_find_all(x, "./fichero"))



}

tidy_fichaGenEnmiendas <- function(x) {

}
