url_diputado <- function(diputado, legislatura) {

    if (!is.numeric(diputado)) {
        stop("diputado should be a numeric value", call. = FALSE)
    }

    if (any(diputado > 350)) {
        stop("diputado cannot be higher than 349", call. = FALSE)
    }
    if (!is.numeric(legislatura)) {
        stop("diputado should be a numeric value", call. = FALSE)
    }
    n_diputado <- length(diputado)
    n_legislatura <- length(legislatura)
    if (n_diputado > 1 || n_legislatura > 1) {
        length_total <- n_diputado*n_legislatura
        diputado <- rep(diputado, length.out = length_total)
        legislatura <- rep(legislatura, length.out = length_total)
    }
  paste0("http://www.congreso.es/portal/page/portal/Congreso/Congreso/",
  "Diputados/BusqForm?_piref73_1333155_73_1333154_1333154.next_page=",
  "/wc/fichaDiputado&idDiputado=", diputado, "&idLegislatura=", legislatura)
}


# # Dates of registrary and down
# xml_find_all(curriculum, "//div[@class='dip_rojo']")[3:4]
