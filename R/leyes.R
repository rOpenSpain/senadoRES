#' Approved laws
#'
#' Check how many laws were approved.
#' @inheritParams mociones
#' @return A data.frame with the approved laws and related information.
#' @examples
#' if (interactive()) {
#'     l <- leyes(14)
#' }
leyes <- function(legislatura) {
    url <- "https://www.senado.es/web/ficopendataservlet?tipoFich=18&legis="
    url <- paste0(url, legislatura)
    xml <- get_xml(url, encoding = "UTF-8")
    leyes <- xml_find_all(xml, ".//detalleLey")
    organicas <- xml_find_all(xml, ".//detalleLeyOrganica")
    tipo <- c(rep("Ley", length(leyes)),
              rep("Ley Orgánica", length(organicas)))
    o <- lapply(organicas, function(x){trimws(xml2matrix(x))})
    l <- lapply(leyes, function(x){trimws(xml2matrix(x))})
    o2 <- do.call(rbind, o)
    l2 <- do.call(rbind, l)
    out <- rbind.data.frame(l2, o2)
    out$tipo <- tipo
    out
}
