#' *Comisiones*
#'
#' Look up the *comisiones* and *ponencias* of a legislature.
#' Rellevant to know the code of the work groups.
#' @inheritParams plenarias
#' @return A `data.frame` with the *comisiones* and *ponencias* of that legislature.
#' @export
#' @examples
#' if (interactive()) {
#'    comisiones(13)
#' }
comisiones <- function(legislatura) {
    if (!is_numeric(legislatura) || length(legislatura) > 1) {
        stop("Should be a single number, like 1 or 14")
    }

    url <- "https://www.senado.es/web/ficopendataservlet?tipoFich=7&legis="
    xml <- get_xml(paste0(url, legislatura), "UTF-8")
    comisiones <- xml_find_all(xml, ".//comision")
    l <- lapply(comisiones, function(x){
        y <- xml_find_all(x, "./*[not(self::comision)]")
        o <- xml2matrix2(y)
        # Fix some errors when code is on the nombreBreve
        if (startsWith(o[, "nombreBreve"], "S")) {
            colnames(o)[1:3] <- c("codigo", "nombre", "nombreBreve")
        }
        o
    })
    m <- Reduce(merger, l)
    out <- as.data.frame(m, make.names = FALSE)
    out$legislatura <- legislatura
    out
}
