
#' @param A numeric id.
#' @param legislatura A numeric value of the legislature
#' @examples
#' url_diputado(10, 13)
url_diputado <- function(diputado, legislatura) {

    if (!is_numeric(diputado)) {
        stop("diputado should be a numeric value", call. = FALSE)
    }

    if (any(as.numeric(diputado) >= 350)) {
        stop("diputado cannot be higher than 349", call. = FALSE)
    }
    if (!is_numeric(legislatura)) {
        stop("legislatura should be a numeric value", call. = FALSE)
    }
    n_diputado <- length(diputado)
    n_legislatura <- length(legislatura)
    # Get all combinations
    if (n_diputado > 1 || n_legislatura > 1 && n_diputado != n_legislatura) {
        a <- expand.grid(diputado, legislatura)
        diputado <- a[, 1]
        legislatura <- a[, 1]
    }
  paste0("http://www.congreso.es/portal/page/portal/Congreso/Congreso/",
  "Diputados/BusqForm?_piref73_1333155_73_1333154_1333154.next_page=",
  "/wc/fichaDiputado&idDiputado=", diputado, "&idLegislatura=", legislatura)
}


# # Dates of registrary and down
# xml_find_all(curriculum, "//div[@class='dip_rojo']")[3:4]
