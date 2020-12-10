#' Retrieve the document of _sumario_
#'
#' Returns the summary of a session in a tidy way.
#' If multiple autors are involved in a disposicion and multiple documents they
#' get mixed.
#' @param cve The character of the CVE of the document.
#' @return A data.frame with a summary of the session, date, documents, authors.
#' @family types of documents
#' @export
#' @examples
#' sumario_csv <- sumario_csv(14, 3)
#' if (interactive()) {
#'     head(sumario(sumario_csv))
#' }
sumario <- function(cve) {
    check_code(cve)
    cve <- fix_sumario_code(cve) # Fix till they fix on the website
    # For some reason the xml files of the sumarios are on the pdf folder too...
    url <- paste0("/legis", id2legis(cve), "/publicaciones/pdf/senado/bocg/",
                  cve, ".XML")
    base_url <- force(BASE_URL)
    url <- httr::modify_url(base_url, path = url)
    xml <- get_xml(url)

    header <- xml2matrix(xml_find_all(xml, ".//cabecera"))
    apartados <- xml_find_all(xml, ".//cuerpo/apartados/apartado")
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

tidy_apartado <- function(x) {
    subapartados <- xml_children(xml_find_all(x, ".//subapartado"))
    apartado <- xml2matrix2(subapartados[[1]])
    disposiciones <- xml_children(subapartados[[2]])

    a <- lapply(disposiciones, tidy_disposicion)
    b <- Reduce(merger, a)
    head <- xml2matrix2(xml_find_all(x, ".//apNumRomano|apDescripcion"))
    cbind.data.frame(add_rows(head, b), add_rows(apartado, b), b)
}

tidy_disposicion <- function(x) {
    info <- xml2matrix2(
        xml_find_all(x, "./objeto|tipoExpediente|disp|cve|fase|extra"))
    info <- as.data.frame(info, row.names = NULL, make.names = FALSE)
    autor <- xml_text(xml_find_all(x, ".//autor"))
    if (length(autor) != 0) {
        # Need to find a document with authors
        # Set a warning to remind myself
        info$autor <- list(autor)
    }
    info
}

#' Boletin
#'
#' @param cve A character with the boletin CVE.
#' @export
#' @return A data.frame
#' @seealso [boletin_csv()]
#' @examples
#' boletin_csv <- boletin_csv(14, 3)
#' b <-  boletin(boletin_csv)
boletin <- function(cve) {
    # /web/actividadparlamentaria/publicacionesoficiales/senado/boletinesoficiales/detalle/index.html?id=20112020
    #  Follows the DDMMYYYY format
    check_code(cve)
    url <- paste0(force(BASE_URL), "/legis", id2legis(cve),
                  "/publicaciones/xml/senado/bocg/", cve, ".XML")
    xml <- get_xml(url, encoding = "ISO-8859-15")
    cabecera <- xml2matrix2(xml_children(xml_find_all(xml, "./cabecera"))[1:6])
    disposiciones <- xml_find_all(xml, "./texto_boletin/disposicion")
    dis <- lapply(disposiciones, tidy_long_disposicion)
    dis <- Reduce(rbind, dis)
    out <- cbind.data.frame(add_rows(cabecera, dis), dis)
    numerics <- c("NBOL", "ANNO", "numpag", "numpagfin", "DISP")
    out[, numerics] <- lapply(out[, numerics], as.numeric)
    out$FECHA <- as.Date(out$FECHA, "%Y%m%d")
    ids <- paste(out$CVE, out$DISP, sep = "_")
    ids <- change_type_document(ids, "D")
    out$document <- ids
    out
}

#' Info of a session
#'
#' Merges both information from the [boletin()] and from the [sumario()]
#' @inheritParams sumario_csv
#' @return A data.frame
#' @seealso [boletin()] and [sumario()]
#' @export
#' @examples
#' bs <- boletin_sumario(14, 3)
boletin_sumario <- function(legislatura, sesion) {

    s <- sumario(sumario_csv(legislatura, sesion))
    b <- boletin(boletin_csv(legislatura, sesion))
    out <- cbind(s, b)
    out
}

#' A document
#' @param cve A character with a document CVE.
#' @family types of documents
#' @seealso [document_csv()]
#' @export
#' @examples
#' document_csv <- "BOCG_D_14_110_901"
#' documento(document_csv)
documento <- function(cve) {
    check_code(cve)
    url <- paste0("https://www.senado.es/legis",
                  id2legis(cve), "/publicaciones/xml/senado/bocg/", cve, ".XML")
    xml <- read_xml(url)
    cabecera <- xml2matrix2(xml_children(xml_find_all(xml, "./cabecera"))[1:9])
    disposicion <- tidy_long_disposicion(xml_children(
        xml_child(xml, "texto_boletin")))
    out <- as.data.frame(cbind(cabecera, disposicion), make.names = FALSE)
    out$FECHA <- as.Date(out$FECHA, "%Y%m%d")
    numerics <- c("NBOL", "ANNO", "numpag", "numpagfin", "DISP")
    out[, numerics] <- lapply(out[, numerics], as.numeric)
    out
}

#' @importFrom xml2 xml_attrs
#' @importFrom xml2 xml_child
tidy_long_disposicion <- function(x) {

    cabecera <- xml_child(x, "cabecera_disposicion")
    header <- xml2matrix2(xml_children(cabecera)[1:2])
    attrs <- xml_attrs(x)
    attrs <- if (is.list(attrs)) {attrs[[1]]}else {attrs}
    header <- cbind.data.frame(header, t(as.matrix(attrs)),
                    xml2matrix(xml_child(cabecera, "item_publicado")))

    header$text <- xml_text(xml_child(x, "texto_disposicion"))
    header
}
