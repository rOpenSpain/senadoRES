#' Votos
#'
#' Retrieve votes in a sesion.
#' @inheritParams leyes
#' @param sesion The number of the meeting.
#' @param votacion Number of the vote.
#' @returns A votos class with information about who voted what.
#' @export
#' @examples
#' \donttest{
#' votos_64 <- votos(14, 64)
#' votos_3_8 <- votos(14, 3, 8)
#' }
votos <- function(legislatura, sesion, votacion = NULL) {
    # if (!endsWith(x, ".xml")) {
    #     url <- paste0(force(BASE_URL), x)
    #     "https://www.senado.es/web/actividadparlamentaria/sesionesplenarias/pleno/rwdsesionespleno/detalle/index.html?id=79&legis=10&ns=76&vot=vot"
    # }

    if (!is_numeric(legislatura) && !is_integer(legislatura)) {
        stop("")
    }
    if (!is_numeric(sesion) && !is_integer(sesion)) {
        stop("")
    }
    if (!is_numeric(votacion) && !is_integer(votacion)) {
        stop("")
    }
    pre_url <- paste0(force(BASE_URL), "/legis", legislatura,
                      "/votaciones/ses_", sesion)
    if (is.null(votacion)) {
        url <- paste0(pre_url, ".xml")
    } else {
        url <- paste0(pre_url, "_", votacion, ".xml")
    }
    r <- read_xml(url)
    votaciones <- xml_find_all(r, "/main/sesion/votacion")
    s <- sesion(r)
    s2 <- cbind(legislatura = legislatura, s)
    vot <- lapply(votaciones, votacion)
    l <- list(sesion = s2, votos = vot)
    class(l) <- c("votos", class(l))
    l
}

#' @export
print.votos <- function(x, ...) {
    cat("Legislatura:", x$sesion$legislatura, "Sesion:", x$sesion$num_sesion,
        "Votaciones:", length(x$votos))
}

sesion <- function(x) {
    num_sesion <- xml_text(xml_find_all(x, "/main/sesion/num_sesion"))
    fecha_sesion <- xml_text(xml_find_all(x, "/main/sesion/fecha_sesion"))
    diario_sesion <- xml_text(xml_find_all(x, "/main/sesion/diario_sesiones"))
    data.frame(num_sesion = num_sesion, fecha_sesion = fecha_sesion, diario_sesiones = I(list(diario_sesion)))
}

votoSenador <- function(x){
    votoSenador <- xml_find_all(x, ".//VotoSenador")
    l <- lapply(votoSenador, xml2matrix)
    do.call(rbind, l)
}

votacion <- function(x) {
    l <- list(
        num_vot = xml_find_first(x, "num_vot"),
        CodVotacion = xml_find_first(x, "CodVotacion"),
        num_exp = xml_find_first(x, "num_exp"),
        tit_vot = xml_find_first(x, "tit_vot"),
        tit_sec = xml_find_first(x, "tit_sec"),
        fecha_v = xml_find_first(x, "fecha_v"),
        hora_vot = xml_find_first(x, "hora_vot"),
        preside_vot = xml_find_first(x, "preside_vot"),
        tot_presentes = xml_find_first(x, "tot_presentes"),
        tot_afirmativos = xml_find_first(x, "tot_afirmativos"),
        tot_negativos = xml_find_first(x, "tot_negativos"),
        tot_abstenciones = xml_find_first(x, "tot_abstenciones"),
        tot_novotan = xml_find_first(x, "tot_novotan"),
        tot_nulos = xml_find_first(x, "tot_nulos"),
        tot_ausentes = xml_find_first(x, "tot_ausentes")
    )
    l2 <- sapply(l, xml_text)
    votos <- votoSenador(x)
    ausentes <- lapply(xml_find_all(x, "./ausentes/ausencia"), xml2matrix)
    ausentes2 <- do.call(rbind, ausentes)
    new_columna <- setdiff(colnames(votos), colnames(ausentes2))
    m <- matrix(nrow = nrow(ausentes2), ncol = length(new_columna))
    colnames(m) <- new_columna
    ausentes3 <- cbind(ausentes2, m)
    l3 <- list(voto = l2, resultados = rbind(votos,
                                             ausentes3[, colnames(votos)]))
    class(l3) <- c("voto", class(l3))
    l3
}

#' @export
print.voto <- function(x, ...) {
    cat("Legislatura:", x$sesion$num_legislatura, "Sesion:", x$sesion$num_sesion,
        "Votaciones:", length(x$votos))
}
