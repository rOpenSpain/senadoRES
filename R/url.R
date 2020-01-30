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

a <- url_diputado(10, 13)
library(xml2)
y <- read_html(x = a, options = "NOERROR")
x <- xml_find_all(y, "//div[@id='datos_diputado']")
datos_diputado <- xml_children(x)
z <- xml_find_all(datos_diputado, "//meta[@name='Title']")
names <- xml_attr(z, "content")
group <- xml_contents(xml_find_all(datos_diputado, "//p[@class='nombre_grupo']"))

curriculum <- xml_find_all(y, "//div[@id='curriculum']")
xml_find_all(curriculum, "div[@class='texto_dip']")

# Dates of registrary and down
xml_find_all(curriculum, "//div[@class='dip_rojo']")[3:4]
