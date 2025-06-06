% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{COMISSION_config}
\alias{COMISSION_config}
\title{The MEMC model configuration for COMISSION}
\format{
An object created from \code{\link{memc_configure}}, containing the following elements.
\describe{
\item{name}{COMISSION}
\item{table}{table of the dynamics used by this model configuration; see \link{dynamics} for more details.}
\item{params}{data table of the model parameters; see \link{memc_params} for more details.}
\item{state}{vector of the initial state values; see \link{memc_initial_state} for more details.}
}
}
\usage{
COMISSION_config
}
\description{
The COMISSION configuration is based off of Ahrens et al. 2015. The
model uses MM for the DOM uptake, RMM for POM decomposition, and LM
for MB decay; see \link{dynamics} for more details.
}
\examples{
## Use memc_solve to run the simulation using this configuration.
print(COMISSION_config)
memc_solve(mod = COMISSION_config, time = 1:10)
}
\references{
Ahrens et al.: Contribution of sorption, DOC transport and
microbial interactions to the 14C age of a soil organic carbon profile:
Insights from a calibrated process model, Soil Biol. Biochem., 88, 390–402,
2015. \url{https://doi.org/10.1016/j.soilbio.2015.06.008}
}
\seealso{
Other configurations: 
\code{\link{BAMS_config}},
\code{\link{CORPSE_config}},
\code{\link{MEMS_config}},
\code{\link{MEND_config}},
\code{\link{MIMCS_config}}
}
\concept{configurations}
\keyword{datasets}
