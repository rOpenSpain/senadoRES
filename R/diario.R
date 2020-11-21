
document_nbo <- function(legislatura, sesion, type = "S") {

    type <- match.arg(type, c("T", "S"))
    if (!is_numeric(legislatura) || legislatura < 0) {
        stop("legislatura must be a numeric value equal or above 0.",
             call. = FALSE)
    }
    if (!is_numeric(sesion) || sesion <= 0) {
        stop("sesion must be a numeric value equal or above 0.", call. = FALSE)
    }
    paste("BOCG", type, legislatura, sesion, sep = "_" )
}


#' Summary code for a session
#'
#' Creates the code of the summary of a session of the senate.
#' @param legislatura A numeric value. Constituent was 0.
#' @param sesion A numeric value above 0.
#' @return A character id of the code for the summary of that session.
#' @export
#' @examples
#' sumario_nbo(14, 1)
sumario_nbo <- function(legislatura, sesion) {
    document_nbo(legislatura, sesion, "S")
}

#' Boletin's code
#'
#' Creates the code of the summary of a session of the senate with the
#' documents that got talk.
#' @inheritParams sumario_nbo
#' @return Summary of the session plus information about the documents discussed.
#' @export
#' @examples
#' boletin_bno(14, 1)
boletin_nbo <- function(legislatura, sesion) {
    document_nbo(legislatura, sesion, type = "T")
}

#' Retrieve the document of sumario
#'
#' Returns the summary of a session in a tidy way.
#' If multiple autors are involved in a disposicion and multiple documents they
#' get mixed.
#' @param id The character of the CVE of the document.
#' @return A data.frame with a summary of the session, date, documents, authors.
#' @export
#' @examples
#' sumario_id <- sumario_nbo(14, 1)
#' s <- sumario(sumario_id)
sumario <- function(id) {
    check_code(id)
    id <- fix_sumario_code(id) # Fix till they fix on the website
    # XML of sumarios are on the pdf folder.
    url <- paste0("/legis", id2legis(id), "/publicaciones/pdf/senado/bocg/",
                  id, ".XML")
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
#' @param id A character with the boletin CVE.
#' @return A data.frame
#' @examples
#' boletin_id <- boletin_nbo(14, 2)
#' boletin(boletin_id)
boletin <- function(id) {
    check_code(id)
    # For some reason the xml files of the sumarios are on the pdf folder...
    url <- paste0(force(BASE_URL), "/legis", id2legis(id), "/publicaciones/xml/senado/bocg/",
                  id, ".XML")
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

#' A
#' @examples
#' document_cve <- "BOCG_D_14_110_901"
#' documento(document_cve)
documento <- function(id) {
    check_code(id)
    url <- paste0("https://www.senado.es/legis",
                  id2legis(id), "/publicaciones/xml/senado/bocg/", id, ".XML")
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
    attrs <- if (is.list(attrs)) attrs[[1]]
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

#' Check code
#'
#' @param id Code of an official publication of the Senate
#' @return TRUE if passes all the checks
#' @export
#' @examples
#' check_code("BOCG_S_1_1")
#' check_code("BOCG_B_1_1")
#' check_code("BOCG_D_1_1_1")
check_code <- function(id) {
    ids <- strsplit(id, "_", fixed = TRUE)[[1]]
    if (ids[1] != "BOCG") {
        stop("Unrecognized journal", call. = FALSE)
    }

    if (is_numeric(ids[2])) {
        stop("Should be either T, D or S type of document.", call. = FALSE)
    }

    # Documents: Total, Document, Boletin
    # Silently accept B too for summaries and xml download
    if (!ids[2] %in% c("T", "D", "S", "B")) {
        stop("Unrecognized type of document.", call. = FALSE)
    }

    if (!is_numeric(ids[3])) {
        stop("Should be a number", call. = FALSE)
    }

    if (ids[2] %in% c("T") && length(ids) < 4) {
        stop("Missing the session number.", call. = FALSE)
    }

    if (ids[2] == "D" & length(ids) < 4) {
        stop("Missing the document reference", call. = FALSE)
    }
    if (ids[2] == "D" & length(ids) == 4  & !is_numeric(ids[4])) {
        stop("Document reference should be numeric", call. = FALSE)
    }

    TRUE
}
