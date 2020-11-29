#' Political groups and parties
#'
#' Retrieves the data for each term of the groups and political parties present
#' @param legislatura A numeric value above 12 (No information prior to then).
#' @return A matrix with all the information available.
#' @export
#' @examples
#' grupos()
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
