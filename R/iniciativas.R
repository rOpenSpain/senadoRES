
#' Look up the legislative iniciatives
#'
#' @inheritParams document_csv
#' @return A data.frame with the initiatives of that legislature.
#' @seealso iniciativas_parlamentarias
#' @examples
#' if (interactive()) {
#'     iniciativas(14)
#' }
iniciativas <- function(legislatura) {
    if (!is_numeric(legislatura)) {
        stop("Specify a single numeric number")
    }
    if (legislatura < 5) {
        stop("Data not availabe us a higher number.")
    }
    url <- "https://www.senado.es/web/ficopendataservlet?tipoFich=9&legis="

    xml <- get_xml(paste0(url, legislatura), encoding = "UTF-8")
    iniciativas <- xml_find_all(xml, "./iniciativa")
    out <- lapply(iniciativas, tidy_iniciativas)
    Reduce(merger, out)
}

tidy_iniciativas <- function(x){
    out <- xml2matrix2(xml_find_all(x,
                            paste0("./", omit_xml(c("fichero", "votaciones", "listaEnmiendas")))))
    out <- as.data.frame(out)
    votaciones <- lapply(xml_find_all(x, "./votaciones/votacion"), tidy_votacion)
    if (length(votaciones) != 0) {
        vot <- do.call(rbind, votaciones)
        url_votacion <- xml_text(xml_find_first(x,
                                                "./votaciones/fichGenVotaciones/fichUrlVotaciones"))
        vot$fichUrlVotaciones <- rep(url_votacion, nrow(vot))
        out$votaciones <- list(vot)
    }
    fichero <- xml_text(xml_find_all(x, "./fichero/fichUrl"))
    url_enmiendas <- xml_text(xml_find_all(x, "./listaEnmiendas/fichGenEnmiendas/fichUrlEnmiendas"))
    if (length(fichero) != 0) {
        out$fichero <- fichero
    }
    if (length(url_enmiendas) != 0) {
        out$enmiendas <- url_enmiendas
    }
    out
}

tidy_votacion <- function(x) {
    xml_votacion <- xml_text(xml_find_all(x, ".//fichUrlVotacion"))
    url_votacion <- xml_text(xml_find_all(x, ".//urlVotacion"))
    titulo_votacion <- xml_text(xml_find_all(x, ".//tituloVotacion"))
    data.frame(tituloVotacion = titulo_votacion, urlVotacion = url_votacion, fichUrlVotacion = xml_votacion)
}
