tidy_partido <- function(y) {
    names <- xml_name(xml_children(y))
    txt <- xml_text(xml_children(y))
    names(txt) <- names
    txt
}

tidy_cabecera <- function(y) {
    names <- xml_name(xml_children(y))
    txt <- xml_text(xml_children(y))
    names(txt) <- names
    txt
}


#' Political groups and parties
#'
#' Retrieves the data for each term of the groups and political parties present
#' @param legislatura A numeric value above 12 (No information prior to then).
#' @return A matrix with all the information available.
#' @export
#' @examples
#' grupos()
grupos <- function(legislatura = 12){
    stopifnot(legislatura >= 12)
    x <- paste0("https://www.senado.es/web/ficopendataservlet?tipoFich=4&legis=", legislatura)

    x <- xml_children(read_xml(download_xml(x)))
    out <- lapply(x, function(y){
        a <- sapply(xml_find_all(y, ".//partido"), tidy_partido)
        a <- t(a)
        b <- sapply(xml_find_all(y, ".//datosCabecera"), tidy_cabecera)
        b <- t(b)
        cbind(a, b[rep(1, nrow(a)), , drop = FALSE])
    })
    browser()
    do.call(rbind, out)
}

#' Organization chart
#'
#' Retrieves the relationships between people responsible
#' @return A `data.frame` with all the information available.
#' @export
#' @examples
#' head(organigrama())
organigrama <- function(){
    x <- download_xml("https://www.senado.es/web/ficopendataservlet?tipoFich=5")
    x <- read_xml(x)
    s <- data.frame(
        nivel = xml_text(xml_find_all(x, "//dependencia/nivel")),
        nombreOficial = xml_text(xml_find_all(x, "//dependencia/nombreOficial")),
        nombre = xml_text(xml_find_all(x, "//dependencia/nombre")),
        codigo = xml_text(xml_find_all(x, "//dependencia/codigo")),
        codigoPadre = xml_text(xml_find_all(x, "//dependencia/codigoPadre")),
        tipoDependencia = xml_text(xml_find_all(x, "//dependencia/tipoDependencia")),
        jefe = xml_text(xml_find_all(x, "//dependencia/jefe")),
        dirCorreo = xml_text(xml_find_all(x, "//dependencia/dirCorreo")),
        numTelefono = xml_text(xml_find_all(x, "//dependencia/numTelefono"))
    )
    s
}


#' Senators
#'
#' Past and current appointed members.
#' @return A `data.frame` with the information available
#' @export
#' @examples
#' head(senadores())
senadores <- function() {
    base_url <- "https://www.senado.es/web/ficopendataservlet?tipoFich=10"
    if (is.null(legislatura)) {
        x <- download_xml(base_url)
    } else {
        x <- download_xml(paste0(base_url, "&legis=", legislatura))
    }
    x <- read_xml(x)
    s <- data.frame(nombre = xml_text(xml_find_all(x, "//senador/nombre")),
    apellidos = xml_text(xml_find_all(x, "//senador/apellidos")),
    legislatura = as.numeric(xml_text(xml_find_all(x, "//senador/legislatura"))),
    ultCredencial = xml_text(xml_find_all(x, "//senador/ultCredencial")),
    procedTipo = xml_text(xml_find_all(x, "//senador/procedTipo")),
    procedLiteral = xml_text(xml_find_all(x, "//senador/procedLiteral")),
    procedLugar = xml_text(xml_find_all(x, "//senador/procedLugar")),
    grupoCod = xml_text(xml_find_all(x, "//senador/grupoCod")),
    grupoSiglas = xml_text(xml_find_all(x, "//senador/grupoSiglas")),
    grupoNombre = xml_text(xml_find_all(x, "//senador/grupoNombre")))
    s$procedLugar <- gsub(pattern = ".+: ", "", s$procedLugar)
    s$grupoCod <- gsub(pattern = ".+: ", "", s$grupoCod)
    s$sex <- NA
    s$sex[endsWith(s$procedLiteral, "a")] <- "female"
    s$sex[endsWith(s$procedLiteral, "o")] <- "male"
    s
}


#' Clean the data of the diputado
#'
#' @param url The url of the diputado you want to check.
#' @return A vector with the information available.
#' @export
#' @examples
#' a <- url_diputado(10, 13)
#' tidy_diputado(a)
#' @importFrom xml2 read_html
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_contents
#' @importFrom xml2 xml_find_first
#' @importFrom xml2 xml_length
tidy_diputado <- function(url){
    web_dip <- read_html(x = url, options = "NOERROR")
    is_empty <- xml_find_all(web_dip, "//div[@class='SUBTITULO_CONTENIDO']")
    if (xml_length(is_empty) != 0) {
        stop("Empty page.")
    }
    x <- xml_find_all(web_dip, "//div[@id='datos_diputado']")
    datos_diputado <- xml_children(x)
    z <- xml_find_all(datos_diputado, "//meta[@name='Title']")
    names <- xml_attr(z, "content")
    group <- as.character(xml_contents(
        xml_find_all(datos_diputado, "//p[@class='nombre_grupo']")))
    curriculum <- xml_find_all(web_dip, "//div[@id='curriculum']")
    legislatura <- as.character(
        xml_contents(xml_find_first(curriculum, "//div[@class='principal']")))
    legislatura <- strsplit(legislatura, "-| ", fixed = FALSE)[[1]]
    legislatura <- as.numeric(as.roman(legislatura[1]))
    begin_legislatura <- as.numeric(legislatura[4])
    end_legislatura <- as.numeric(legislatura[5])
    "//li/div[@class='dip_rojo']"

    # Declaración de actividades
    # Declaración de Bienes y Rentas
    "//li[@class='regact_dip']/a[@href]"

    res <- c(names, group, legislatura, begin_legislatura, end_legislatura)
    names(res) <- c("Name", "Party", "Legislature", "Begin", "End")
    res
}

# Resumen de las diferentes publicaciones...
# http://www.congreso.es/portal/page/portal/Congreso/Congreso/Publicaciones/IndPub


# Senado!!
# http://www.senado.es/web/actividadparlamentaria/publicacionesoficiales/senado/boletinesoficiales/index.html


# Senadores::
# http://www.senado.es/web/relacionesciudadanos/datosabiertos/catalogodatos/index.html

# BOCG electronico a partir del 1 de enero de 2011
# diarios de sesiones y boletines oficiales, a partir de 1 de agosto de 2012,
# tendrá consideración de copia auténtica si incluye el CVE.

#  El buscador puede ir bien para sacar codigos:
# http://www.senado.es/buscador/page/senado-lst-avanzada/sensearch?q=&sc=bofi%2Cdise&sf=&sq=mssearch_fld352%3ADS&off=0&dp=20&stem=false&is=&tes=true&trel=&od=&originForm=senado-form-publicaciones&sort=&ff=&ff=msstored_sfc_camara%3ASenado&ff=msstored_sfc_camara%3ACongreso&ff=msstored_sfc_camara%3ACortes+Generales
# Cámara: CONG/DS/CORT
# Serie: C/P/CM/CI/DP/S
# http://www.congreso.es/public_oficiales/L14/CORT/BOCG/A/BOCG-14-CG-A-1.PDF
# http://www.congreso.es/public_oficiales/L14/CORT/BOCG/B/BOCG-14-CG-B-3.PDF
# http://www.congreso.es/public_oficiales/L14/CORT/BOCG/B/BOCG-14-CG-B-2.PDF
# http://www.congreso.es/public_oficiales/L14/CORT/BOCG/B/BOCG-14-CG-B-1.PDF
# http://www.congreso.es/public_oficiales/L14/SEN/BOCG/2020/BOCG_D_14_10_304.PDF
# http://www.senado.es/legis14/publicaciones/pdf/senado/ds/DS_P_14_1.PDF
# http://www.senado.es/legis13/publicaciones/pdf/senado/ds/DS_P_13_1.PDF
# http://www.senado.es/legis13/publicaciones/xml/senado/ds/DS_P_13_1.XML
# http://www.congreso.es/portal/page/portal/Congreso/Congreso/Publicaciones/DiaSes/Pleno
# http://www.congreso.es/public_oficiales/L14/CONG/DS/PL/DSCD-14-PL-2.PDF
# http://www.congreso.es/public_oficiales/L14/SEN/BOCG/2020/BOCG_D_14_10_304.PDF
# http://www.congreso.es/public_oficiales/L14/CONG/DS/CO/DSCD-13-PL-14.PDF
# http://www.congreso.es/public_oficiales/L14/CONG/DS/CO/DSCD-13-PL-15.PDF
# http://www.congreso.es/public_oficiales/L14/CONG/DS/CO/DSCD-14-PL-1.PDF
# http://www.congreso.es/public_oficiales/L14/CONG/DS/CO/DSCD-14-PL-1-C1.PDF
# http://www.congreso.es/l14p/e0/e_0001088_n_000_2601.pdf



# http://www.senado.es/web/ficopendataservlet?tipoFich=14&legis=14#
