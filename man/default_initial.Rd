% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aadoc.R
\docType{data}
\name{default_initial}
\alias{default_initial}
\title{The default initial carbon pool values the basic MEMC model configurations from Wang et al. 2013}
\format{
A vector of the initial states of the Carbon pools from MEND 2013 \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
\describe{
\item{P}{Particulate organic carbon or POC}
\item{M}{Mineral associated organic carbon or MOC, note that this excludes Q}
\item{Q}{Adsorbed phase of DOC}
\item{B}{Microbial biomass carbon or MBC}
\item{D}{Dissolved organic carbon or DOC}
\item{EP}{Enzyme for decomposition of P}
\item{EM}{Ensyme for decomposition of M}
\item{IC}{Accumulated CO2 flux}
\item{Tot}{Total carbon in the patch}
}
}
\usage{
default_initial
}
\description{
The default initial carbon pool values the basic MEMC model configurations from Wang et al. 2013
}
\examples{
default_initial
print(default_initial)
}
\references{
\href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
}
\concept{initial values}
\keyword{datasets}