
#' Retrieve the document of sumario
#'
#' Returns the summary of a session in a tidy way.
#' If multiple autors are involved in a disposicion and multiple documents they
#' get mixed.
#' @param cve The character of the CVE of the document.
#' @return A data.frame with a summary of the session, date, documents, authors.
#' @family types of documents
#' @export
#' @examples
#' sumario_cve <- sumario_nbo(14, 1)
#' head(sumario(sumario_cve))
sumario <- function(cve) {
    check_code(cve)
    cve <- fix_sumario_code(cve) # Fix till they fix on the website
    # XML of sumarios are on the pdf folder.
    url <- paste0("/legis", id2legis(cve), "/publicaciones/pdf/senado/bocg/",
                  cve, ".XML")
    base_url <- force(BASE_URL)
    url <- httr::modify_url(base_url, path = url)
    xml <- get_xml(url)

    header <- xml2matrix(xml_find_all(xml, "/boletin/cabecera"))
    apartados <- xml_find_all(xml, "/boletin/cuerpo/apartados/apartado")
    # Based on the disposition we merge the apartados
    if (length(xml_find_all(xml, "//disposicion")) > 1) {
        apartados <- lapply(apartados, tidy_apartado)
        apartados <- Reduce(merger, apartados)
    } else {
        apartados <- tidy_apartado(apartados)
    }
    out <- cbind(add_rows(header, apartados), apartados)
    out <- as.data.frame(out, make.names = FALSE)
    out$fechaBol <- as.Date(out$fechaBol, "%d/%m/%Y")
    numerics <- c("idlegislatura", "idnumero", "tipoExpediente")
    out[, numerics] <- lapply(out[, numerics], as.numeric)
    out
}

#' Boletin
#'
#' @param cve A character with the boletin CVE.
#' @export
#' @return A data.frame
#' @seealso [boletin_cve()]
#' @examples
#' boletin_CVE <- boletin_cve(14, 2)
#' boletin(boletin_CVE)
boletin <- function(cve) {
    check_code(cve)
    # For some reason the xml files of the sumarios are on the pdf folder...
    url <- paste0(force(BASE_URL), "/legis", id2legis(cve), "/publicaciones/xml/senado/bocg/",
                  cve, ".XML")
    xml <- get_xml(url)
    cabecera <- xml2matrix2(xml_children(xml_find_all(xml, "./cabecera"))[1:6])
    disposiciones <- xml_find_all(xml, "./texto_boletin/disposicion")
    dis <- lapply(disposiciones, tidy_long_disposicion)
    dis <- Reduce(rbind, dis)
    out <- as.data.frame(cbind(add_rows(cabecera, dis), dis),
                         make.names = FALSE)
    numerics <- c("NBOL", "ANNO", "numpag", "numpagfin", "DISP")
    out[, numerics] <- lapply(out[, numerics], as.numeric)
    out$FECHA <- as.Date(out$FECHA, "%Y%m%d")
    out
}

#' A document
#' @param cve A character with a document CVE.
#' @family types of documents
#' @seealso [document_cve()]
#' @export
#' @examples
#' document_cve <- "BOCG_D_14_110_901"
#' documento(document_cve)
documento <- function(cve) {
    check_code(cve)
    url <- paste0("https://www.senado.es/legis",
                  id2legis(cve), "/publicaciones/xml/senado/bocg/", cve, ".XML")
    xml <- read_xml(url)
    cabecera <- xml2matrix2(xml_children(xml_find_all(xml, "./cabecera"))[1:9])
    disposicion <- tidy_long_disposicion(xml_children(xml_child(xml, "texto_boletin")))
    out <- as.data.frame(cbind(cabecera, disposicion), make.names = FALSE)
    out$FECHA <- as.Date(out$FECHA, "%Y%m%d")
    numerics <- c("NBOL", "ANNO", "numpag", "numpagfin", "DISP")
    out[, numerics] <- lapply(out[, numerics], as.numeric)
    out
}


tidy_apartado <- function(x) {
    subapartados <- xml_children(xml_find_all(x, ".//subapartado"))
    apartado <- xml2matrix2(subapartados[[1]])
    disposiciones <- xml_children(subapartados[[2]])

    a <- lapply(disposiciones, tidy_disposicion)
    disposiciones <- Reduce(merger, a)
    head <- xml2matrix2(xml_find_all(x, ".//apNumRomano|apDescripcion"))

    cbind(add_rows(head, disposiciones),
          add_rows(apartado, disposiciones),
          disposiciones)
}

#' @importFrom xml2 xml_attrs
#' @importFrom xml2 xml_child
tidy_long_disposicion <- function(x) {

    cabecera <- xml_child(x, "cabecera_disposicion")
    header <- xml2matrix2(xml_children(cabecera)[1:2])
    attrs <- xml_attrs(x)
    attrs <- if (is.list(attrs)) {attrs[[1]]}else {attrs}
    header <- cbind(header, t(as.matrix(attrs)), # Doesn't work well if it is a nodeset
                    xml2matrix(xml_child(cabecera, "item_publicado")))

    text <- xml_text(xml_child(x, "texto_disposicion"))
    cbind(header, text = text)
}

tidy_disposicion <- function(x) {
    autor <- t(xml2matrix2(xml_find_all(x, ".//autor")))
    colnames(autor) <- unique(rownames(autor))
    ficheros <- t(xml2matrix2(xml_find_all(x, ".//fichero")))
    colnames(ficheros) <- unique(rownames(ficheros))
    info <- xml2matrix2(
        xml_find_all(x, "./objeto|tipoExpediente|disp|cve|fase|extra"))
    pick <- if (nrow(autor) > nrow(ficheros)) autor else ficheros
    cbind(add_rows(info, pick),
          add_rows(ficheros, pick),
          add_rows(autor, pick))
}

