% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diario.R
\name{boletin}
\alias{boletin}
\title{Boletin}
\usage{
boletin(cve)
}
\arguments{
\item{cve}{A character with the boletin CVE.}
}
\value{
A data.frame
}
\description{
Boletin
}
\examples{
boletin_csv <- boletin_csv(14, 3)
b <-  boletin(boletin_csv)
}
\seealso{
\code{\link[=boletin_csv]{boletin_csv()}}
}
