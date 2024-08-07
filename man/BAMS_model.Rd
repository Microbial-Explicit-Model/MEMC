% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{BAMS_model}
\alias{BAMS_model}
\title{The MEMC model configuration for BAMS}
\format{
an object created form \code{configure_model}, containing the following elements.
\describe{
\item{name}{BAMS}
\item{table}{table of the dynamics used by this model configuration, see \link{dynamics} for more details.}
\item{params}{data table of the model parameters, see \link{default_params} for more details.}
\item{state}{vector of the initial state values, see \link{default_initial} for more details.}
}
}
\usage{
BAMS_model
}
\description{
The BAMS configuration is based off of Tang et al. 2022, this model uses MM for the DOM
uptake, MM for POM decomposition, and a LM for MB decay, see \link{dynamics} for more
details.
}
\examples{
## Use solve_model to run the simulation using this configuration.
print(BAMS_model)
solve_model(mod = MEMS_model, time = 1:10)
}
\references{
\href{https://doi.org/10.5194/bg-16-1225-2019}{Robertson et al. 2019}
}
\seealso{
Other {configurations}: 
\code{\link{COMISSION_model}},
\code{\link{CORPSE_model}},
\code{\link{MEMS_model}},
\code{\link{MEND_model}},
\code{\link{MIMCS_model}},
\code{\link{model_configs}}
}
\concept{{configurations}}
\keyword{datasets}
