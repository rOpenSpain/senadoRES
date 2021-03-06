% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diario.R
\name{boletin_sumario}
\alias{boletin_sumario}
\title{Info of a session}
\usage{
boletin_sumario(legislatura, sesion)
}
\arguments{
\item{legislatura}{A numeric value. Constituent was 0.}

\item{sesion}{A numeric value above 0.}
}
\value{
A data.frame
}
\description{
Merges both information from the \code{\link[=boletin]{boletin()}} and from the \code{\link[=sumario]{sumario()}}
}
\examples{
bs <- boletin_sumario(14, 3)
}
\seealso{
\code{\link[=boletin]{boletin()}} and \code{\link[=sumario]{sumario()}}
}
