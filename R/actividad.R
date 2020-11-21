# Pleno y diputación permanente
    # https://www.senado.es/web/ficopendataservlet?tipoFich=14&legis=13#

#' Plenary sessions
#'
#' How many session have been
#' @export
#' @inheritParams grupos
#' @return A data.frame with the information available.
#' @examples
#' head(plenarias())
plenarias <- function(legislatura = 10) {
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


helper <- function(x, path){
    y <- xml2matrix(xml_find_all(x, paste0("./", path)))
    if (!is.null(dim(y))) {
        colnames(y)[1] <- paste0(path, "_", colnames(y)[1])
    }
    y
}

tidy_asunto <- function(asunto){
    int <- xml_find_all(asunto, "./intervencion")
    l <- lapply(int, function(i) {
        orador <- helper(i, "orador")
        fase <- helper(i, "fase")
        cargo <- helper(i, "cargo")
        grupo <- helper(i, "grupo")

        data <- xml2matrix2(xml_find_all(i,
                                         "./id|asunto|hora_inicio|offset_inicio|hora_fin|duracion_mseg|duracion_texto|path1|path0"))
        cbind(data, orador, fase, cargo, grupo)
    })

    l2 <- do.call(rbind, l)
    colnames(l2)[1] <- paste0("intervencion_", colnames(l2)[1])
    m <- xml2matrix(xml_find_all(asunto, "./*[not(self::intervencion)]"))
    cbind(add_rows(m, l2), l2)
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
    values <- xml_text(meta)
    names(values) <- xml_name(meta)
    asuntos <- xml_find_all(x, "/sesion/asunto")
    l <- lapply(asuntos, tidy_asunto)
    do.call(rbind, l)
}
# Comisiones y Ponencias
# Publicaciones Oficiales
# Iniciativas parlamentarias Legislativas
# Iniciativas parlamentarias de control
