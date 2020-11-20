#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_name
xml2matrix <- function(y) {
    names <- xml_name(xml_children(y))
    txt <- xml_text(xml_children(y))
    names(txt) <- names
    t(txt)
}

xml2matrix2 <- function(y) {
    names <- xml_name(y)
    txt <- xml_text(y)
    names(txt) <- names
    t(txt)
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

    x <- xml_children(read_xml(x))
    out <- lapply(x, function(y){
        partidos <- sapply(xml_find_all(y, ".//partido"), xml2matrix)
        grupo <- sapply(xml_find_all(y, ".//datosCabecera"), xml2matrix)
        cbind(partidos, grupo[rep(1, nrow(partidos)), , drop = FALSE])
    })
    browser()
    do.call(rbind, out)
}



#' Senators
#'
#' Past and current appointed members.
#' @return A `data.frame` with the information available
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_text
#' @export
#' @examples
#' head(senadores())
senadores <- function() {
    x <- read_xml("https://www.senado.es/web/ficopendataservlet?tipoFich=10")
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
