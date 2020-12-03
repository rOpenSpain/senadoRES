#' Document CVE
#'
#' Code needd to retrieve data about a document.
#' @inheritParams sumario_csv
#' @param number A numeric value of
#' @family generators of CVE
#' @return A character vector of a valid CVE.
#' @export
#' @examples
#' document_csv(14, 1, 1)
document_csv <- function(legislatura, sesion, number) {
    if (!is_numeric(number) && !as.numeric(number) > 0) {
        stop("Number should be a numeric value above 0", call. = FALSE)
    }
    paste(document_nbo(legislatura, sesion, "D"), number, sep = "_")
}


document_nbo <- function(legislatura, sesion, type = "S") {

    type <- match.arg(type, c("T", "S", "D"))
    if (!is_numeric(legislatura) || legislatura < 0) {
        stop("legislatura must be a numeric value equal or above 0.",
             call. = FALSE)
    }
    if (!is_numeric(sesion) || sesion <= 0) {
        stop("sesion must be a numeric value equal or above 0.", call. = FALSE)
    }
    paste("BOCG", type, legislatura, sesion, sep = "_" )
}


#' Summary code for a session
#'
#' Creates the code of the summary of a session of the senate.
#' @param legislatura A numeric value. Constituent was 0.
#' @param sesion A numeric value above 0.
#' @return A character id of the code for the summary of that session.
#' @family generators of CVE
#' @export
#' @examples
#' sumario_csv(14, 1)
sumario_csv <- function(legislatura, sesion) {
    document_nbo(legislatura, sesion, "S")
}

#' Boletin's code
#'
#' Creates the code of the summary of a session of the senate with the
#' documents that got talk.
#' @inheritParams sumario_csv
#' @return Summary of the session plus information about the documents discussed.
#' @family generators of CVE
#' @export
#' @examples
#' boletin_csv(14, 1)
boletin_csv <- function(legislatura, sesion) {
    document_nbo(legislatura, sesion, type = "T")
}


#' Check code
#'
#' @param id Code of an official publication of the Senate
#' @return TRUE if passes all the checks
#' @export
#' @examples
#' check_code("BOCG_S_1_1")
#' check_code("BOCG_B_1_1")
#' check_code("BOCG_D_1_1_1")
check_code <- function(id) {
    ids <- strsplit(id, "_", fixed = TRUE)[[1]]
    if (ids[1] != "BOCG") {
        stop("Unrecognized journal", call. = FALSE)
    }

    if (is_numeric(ids[2])) {
        stop("Should be either T, D or S type of document.", call. = FALSE)
    }

    # Documents: Total, Document, Boletin
    # Silently accept B too for summaries and xml download
    if (!ids[2] %in% c("T", "D", "S", "B")) {
        stop("Unrecognized type of document.", call. = FALSE)
    }

    if (!is_numeric(ids[3])) {
        stop("Should be a number", call. = FALSE)
    }

    if (ids[2] %in% c("T") && length(ids) < 4) {
        stop("Missing the session number.", call. = FALSE)
    }

    if (ids[2] == "D" & length(ids) < 5) {
        stop("Missing the document reference", call. = FALSE)
    }
    if (ids[2] == "D" & length(ids) == 5  & !is_numeric(ids[5])) {
        stop("Document reference should be numeric", call. = FALSE)
    }

    TRUE
}
