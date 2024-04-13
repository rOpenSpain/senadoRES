#' Political groups and parties
#'
#' Retrieves the data for each term of the groups and political parties present
#' @param legislatura A numeric value above 12 (No information prior to then).
#' @return A matrix with all the information available.
#' @export
#' @examples
#' if (interactive()){
#'     grupos(13)
#' }
grupos <- function(legislatura){
    stopifnot(legislatura >= 12)
    x <- paste0("https://www.senado.es/web/ficopendataservlet?tipoFich=4&legis=", legislatura)

    x <- xml_children(read_xml(x))
    out <- lapply(x, function(y){
        partidos <- lapply(xml_find_all(y, ".//partido"), xml2matrix)
        partidos <- do.call(rbind, partidos)
        grupo <- xml2matrix(xml_find_all(y, ".//datosCabecera"))
        cbind(add_rows(grupo, partidos), partidos)
    })
    out <- do.call(rbind, out)
    out <- as.data.frame(out)
    numerics <- c("total", "totalElectos", "totalDesignados",
                  "partidoTotalElectos", "partidoTotalDesignados")
    out[, numerics] <- lapply(out[, numerics], as.numeric)
    out
}



#' Senators since 1977
#'
#' Past and current appointed members.
#' @return A `data.frame` with the information available.
#' @inheritParams plenarias
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_find_all
#' @export
#' @examples
#' if (interactive()) {
#'     head(senadores(12))
#' }
senadores <- function(legislatura = 12:14) {
    stopifnot(!is.numeric(legislatura) || all(legislatura >= 0) || !is.null(legislatura))
    senad_list <- lapply(legislatura, senadores_legis)
    senad_df <- do.call(rbind, senad_list)
    # The API return the same data twice
    # Even there is a person LÓPEZ CONDÉS Legis = 3, ultCredencial 284 that is
    # returned with accent and without it.
    unique(senad_df)
}

# Converted from senadores, iterator
senadores_legis <- function(legislatura) {
    x <- read_xml(compose_url(tipoFich = 10, legis = legislatura))
    s <- data.frame(
        nombre = xml_text(xml_find_all(x, "//senador/nombre")),
        apellidos = xml_text(xml_find_all(x, "//senador/apellidos")),
        legislatura = as.numeric(xml_text(xml_find_all(x, "//senador/legislatura"))),
        ultCredencial = xml_text(xml_find_all(x, "//senador/ultCredencial")),
        procedTipo = xml_text(xml_find_all(x, "//senador/procedTipo")),
        procedLiteral = xml_text(xml_find_all(x, "//senador/procedLiteral")),
        procedLugar = xml_text(xml_find_all(x, "//senador/procedLugar")),
        grupoCod = xml_text(xml_find_all(x, "//senador/grupoCod")),
        grupoSiglas = xml_text(xml_find_all(x, "//senador/grupoSiglas")),
        grupoNombre = xml_text(xml_find_all(x, "//senador/grupoNombre"))
    )
    s$procedLugar <- gsub(pattern = ".+: ", "", s$procedLugar)
    s$grupoCod <- gsub(pattern = ".+: ", "", s$grupoCod)
    s$sex <- NA
    s$sex[endsWith(s$procedLiteral, "a")] <- "female"
    s$sex[endsWith(s$procedLiteral, "o")] <- "male"
    s
}
