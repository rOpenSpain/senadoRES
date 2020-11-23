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

add_rows <- function(x, y) {
    if (length(x) == 0) {
        return(matrix(nrow = nrow(y)))
    }

    if (is.numeric(y)) {
        x[rep(1, y), , drop = FALSE]
    } else if (!is.null(dim(y))) {
        x[rep(1, nrow(y)), , drop = FALSE]
    } else {
        x
    }
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
