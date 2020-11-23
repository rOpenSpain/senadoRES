#' Retrieve the XML content
#'
#' Look up on the internet and get the content
#' @param query A query to the website.
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom httr user_agent
#' @importFrom httr status_code
#' @importFrom httr http_type
get_xml <- function(query) {
    user_agent <- user_agent("https://github.com/llrs/senadoRES")
    response <- GET(query, user_agent)
    if (status_code(response) != 200) {
        stop("Could not retrieve the data.", call. = FALSE)
    }
    if (http_type(response) == "text/html") {
        warning("Missing data.", call. = FALSE)
    }
    if (http_type(response) != "text/xml") {
        stop("API did not find the requested document.", call. = FALSE)
    }
    # Encoding found via the browser
    # BOCG_B_14_110.XML has windows-1252
    # BOCG_T_14_3.XML has ISO-8859-15
    # BOCG_T_14_110.XML has ISO-8859-15
    # Apparently ISO-8859-15 includes the windows-1252
    content(response, encoding = "ISO-8859-15")
}
