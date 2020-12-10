# Clean the data of the diputado on the congress!!
#
# @param diputado A numeric id.
# @param legislatura A numeric value of the legislature
# @return A vector with the information available.
# @examples
# tidy_diputado(10, 13)
# @importFrom xml2 read_html
# @importFrom xml2 xml_find_all
# @importFrom xml2 xml_attr
# @importFrom xml2 xml_contents
# @importFrom xml2 xml_find_first
# @importFrom xml2 xml_length
# @importFrom utils as.roman
tidy_diputado <- function(legislatura, diputado){
    url <- url_diputado(diputado, legislatura)
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
