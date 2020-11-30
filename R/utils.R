# Helper function
is_numeric <- function(x){
    is.numeric(tryCatch(as.numeric(x), warning = function(w){FALSE}))
}

id2legis <- function(x){
    strsplit(x, "_", fixed = TRUE)[[1]][3]
}

id2type <- function(x){
    strsplit(x, "_", fixed = TRUE)[[1]][2]
}

BASE_URL <- "https://www.senado.es"

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

xml2ch <- function(y) {
    txt <- xml_text(y)
    names(txt) <- xml_name(y)
    txt
}

add_rows <- function(x, y) {

    if (all(is.na(y))) {
        y <- length(y)
    } else if (is.numeric(y)) {
        y <- y
    } else if (!is.null(dim(y))) {
        y <- nrow(y)
    } else if (is.null(y)) {
        y <- 1
    }


    if (length(x) == 0) {
        return(matrix(nrow = nrow(y)))
    }

    z <- which.max(c(y, nrow(x)))

    if (z == 1) {
        z <- rep(1, length.out = y)
    } else {
       z <-  seq(from = 1, to = nrow(x), by = 1)
    }
    x[z, , drop = FALSE]
}

fix_sumario_code <- function(id) {
    # Fix error on the sumario, code is with S (presumably)
    # but we need to use letter B to download it
    # Sent mail on 21/11/2020
    gsub("_S_", "_B_", id)
}


change_type_document <- function(id, to) {
    to <- toupper(to)
    match.arg(to, c("B", "S", "T", "D")) # B is not an official letter
    ids <- strsplit(id, "_", fixed = TRUE)
    vapply(ids, function(x){
        x[2] <- to
        paste(x, collapse = "_")
        }, FUN.VALUE = character(1L))
}

merger <- function(x, y) {
    z <- merge(x, y, all = TRUE, sort = FALSE)
    rem <- apply(z, 2, function(xz){
        sum(is.na(xz), xz %in% character(0L), xz %in% "") == length(xz)
    })
    z[, !rem, drop = FALSE]
}

omit_xml <- function(x){
    omit <- paste("self", x, sep = "::", collapse = "|")
    paste0("*[not(", omit, ")]")
}
