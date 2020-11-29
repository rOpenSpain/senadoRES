# Pleno y diputación permanente
    # https://www.senado.es/web/ficopendataservlet?tipoFich=14&legis=13#

#' Plenary sessions
#'
#' How many session have been
#' @export
#' @inheritParams grupos
#' @return A data.frame with the information available.
#' @examples
#' head(plenarias(10))
plenarias <- function(legislatura) {
    stopifnot(legislatura >= 10)
    base_url <- "https://www.senado.es/web/ficopendataservlet?tipoFich=14&legis="
    url <- paste0(base_url, legislatura)
    x <- read_xml(url)
    sesion <- xml_find_all(x, "//sesionPlenaria")
    l <- lapply(sesion, xml2matrix)
    out <- do.call(rbind, l)
    out <- as.data.frame(out)

    # Deal with dates and locales
    locale <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", locale))
    Sys.setlocale(category = "LC_TIME", locale = "es_ES.UTF-8")

    out$sesionHoraInicio <- as.difftime(out$sesionHoraInicio,'%H:%M')
    out$sesionFechaInicio <- as.Date(out$sesionFechaInicio, format = "%d de %B de %Y")
    out
}


helper <- function(x, path) {
    y <- xml2ch(xml_children(xml_find_all(x, paste0("./", path))))
    if (!is.null(y) && length(y) != 0) {
        names(y) <- paste0(path, "_", names(y))
        y
    }
}

tidy_asunto <- function(asunto) {
    punto <- xml_find_all(asunto, "./punto")
    if (length(punto) != 0) {
        punto <- tidy_punto(xml_children(punto))
    } else {
        punto <- NA
    }
    int <- xml_find_all(asunto, "./intervencion")
    if (length(int) != 0) {
        l <- lapply(int, tidy_intervencion)
        l2 <- Reduce(merger, l)
    } else {
        l2 <- NA
    }
    m <- xml2matrix(xml_find_all(asunto, "./*[not(self::intervencion|self::punto)]"))
    cbind.data.frame(add_rows(m, l2), l2, punto)
}

tidy_punto <- function(x) {
    punto <- xml2matrix2(x)
    colnames(punto) <- paste0("punto_", colnames(punto))
    punto
}

tidy_intervencion <- function(x) {

    orador <- helper(x, "orador")
    fase <- helper(x, "fase")
    cargo <- helper(x, "cargo")
    grupo <- helper(x, "grupo")
    path <- "./id|asunto|hora_inicio|offset_inicio|hora_fin|duracion_mseg|duracion_texto|path1|path0"
    data <- xml2ch(xml_find_all(x, path))
    out <- c(data, orador, fase, cargo, grupo)
    names(out)[1] <- paste0("intervencion_", names(out[1]))
    as.data.frame(t(as.matrix(out)))
}

#' Check individual session
#' @param url A url path to a session.
#' @return A [data.frame] with all the information about a session.
#' @seealso [plenarias()]
#' @export
#' @examples
#' url <-  "https://www.senado.es/web/ficopendataservlet?tipoFich=11&legis=14&org=S000040&numSes=020&numConv=01&fecha=17112020"
#' detalles(url)
detalles <- function(url) {
    x <- read_xml(url)
    meta <- xml_find_all(x, "/sesion/update|fecha|legis")
    meta <- xml2matrix2(meta)
    asuntos <- xml_find_all(x, "/sesion/asunto")
    browser()
    l <- lapply(asuntos, tidy_asunto)
    do.call(rbind, l)
}
# Comisiones y Ponencias
# Publicaciones Oficiales
# Iniciativas parlamentarias Legislativas
# Iniciativas parlamentarias de control
# TODO: continue here with actividades, fix detalles (do not forget to get data about the people.)
