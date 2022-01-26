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
#' @name default_initial
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' default_initial
#' print(default_initial)
"default_initial"

#' MEMC MEND model configuration based on \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#'
#' @format an object created form \code{configure_model}.
#' \describe{
#' \item{name}{MEND}
#' \item{params}{the default MEMC parameter table}
#' \item{state}{the default inital state values for MEMC}
#' \item{carbon_pools_func}{carbon pools based on Wang et al. 2013.}
#' \item{carbon_fluxes_func}{carbon fluxes based on Wang et al. 2013. The MEND_model MEMC
#' configuration uses MM kinetics for DOMdecomp, MM for  POMdecomp, and LM for MBdecay.
#' see \code{\link{kinetics}} for more details.}
#' }
#' @family MEMC configurations
#' @name MEND_model
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' MEND_model
#' solve_model(mod = MEND_model, time = 1:10)
"MEND_model"

#' MEMC COMISSION model configuration based on  \href{https://doi.org/10.1016/j.soilbio.2020.107912}{Ahrens et al. 2020}
#'
#' @format an object created form \code{configure_model}.
#' \describe{
#' \item{name}{COMISSION}
#' \item{params}{the default MEMC parameter table}
#' \item{state}{the default inital state values for MEMC}
#' \item{carbon_pools_func}{carbon pools based on Wang et al. 2013}
#' \item{carbon_fluxes_func}{carbon fluxes based on Wang et al. 2013. The COMISSION_model MEMC
#' configuration uses RMM kinetics for DOMdecomp, MM for  POMdecomp, and LM for MBdecay.
#' see \code{\link{kinetics}} for more details.}
#' }
#' @family MEMC configurations
#' @name COMISSION_model
#' @references \href{https://doi.org/10.1016/j.soilbio.2020.107912}{Ahrens et al. 2020}
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' COMISSION_model
#' solve_model(mod = COMISSION_model, time = 1:10)
"COMISSION_model"


#' MEMC CORPSE model configuration based on \href{https://doi.org/10.1038/nclimate2436}{Sulman et al. 2014}
#'
#' @format an object created form \code{configure_model}.
#' \describe{
#' \item{name}{CORPSE}
#' \item{params}{the default MEMC parameter table}
#' \item{state}{the default inital state values for MEMC}
#' \item{carbon_pools_func}{carbon pools based on Wang et al. 2013}
#' \item{carbon_fluxes_func}{carbon fluxes based on Wang et al. 2013. The CORPSE_model MEMC
#' configuration uses RMM kinetics for DOMdecomp, LM for POMdecomp, and LM for MBdecay.
#' see \code{\link{kinetics}} for more details.}
#' }
#' @family MEMC configurations
#' @name CORPSE_model
#' @references \href{https://doi.org/10.1038/nclimate2436}{Sulman et al. 2014}
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' CORPSE_model
#' solve_model(mod = CORPSE_model, time = 1:10)
"CORPSE_model"



