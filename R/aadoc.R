#### Documentation objects

#' Identifiers for MEMC supported kinetics used
#'
#' These identifiers correspond to the various kinetics users can use in \code{\link{carbon_fluxes}} to
#' select for different kinetic dynamics.
#'
#' @section Kinetics:
#' \describe{
#' \item{MM}{Michaelis–Menten kinetics, add ref}
#' \item{RMM}{Reverse Michaelis-Menten kinetics, add ref}
#' \item{ECA}{TODO}
#' \item{LM}{Linear kinetics model, add ref}
#' }
#' @name kinetics
#' @family test
NULL

#' The parameter values for the basic MEMC model configurations from Wang et al. 2013
#'
#' These identifiers correspond to the various kinetics users can use in \code{\link{carbon_fluxes}} to
#' select for different kinetic dynamics.
#'
#' @format A data table consisting of 4 columns with inputs from \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' @section Columns:
#' \describe{
#' \item{parameter}{String character of the default MEND parameters.}
#' \item{description}{String character describing the parameter.}
#' \item{units}{String character of the parameter units.}
#' \item{value}{Numeric values taken from the table 2 of Wang et al. 2013}
#' }
#' @name default_params
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @usage data(default_params)
#' @keywords datasets
"default_params"

#' The default initial carbon pool values the basic MEMC model configurations from Wang et al. 2013
#'
#' @format A vector of the initial states of the Carbon pools from MEND 2013 \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' \describe{
#' \item{P}{Particulate organic carbon or POC}
#' \item{M}{Mineral associated organic carbon or MOC, note that this excludes Q}
#' \item{Q}{Adsorbed phase of DOC}
#' \item{B}{Microbial biomass carbon or MBC}
#' \item{D}{Dissolved organic carbon or DOC}
#' \item{EP}{Enzyme for decomposition of P}
#' \item{EM}{Ensyme for decomposition of M}
#' \item{IC}{Accumulated CO2 flux}
#' \item{Tot}{Total carbon in the patch}
#' }
#' @family initial values
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' default_initial
#' print(default_initial)
"default_initial"


