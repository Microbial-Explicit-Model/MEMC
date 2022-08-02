#' The parameter values for the basic MEMC model configurations from Wang et al. 2013
#'
#' @docType data
#' @usage data(default_params)
#' @keywords datasets
#'
#' @format A data table consisting of 4 columns with inputs from \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' \describe{
#' \item{parameter}{String character of the default MEND parameters.}
#' \item{description}{String character describing the parameter.}
#' \item{units}{String character of the parameter units.}
#' \item{value}{Numeric values taken from the table 2 of Wang et al. 2013}
#' }
#' @family parameters
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' default_params
"default_params"


#' The default inital carbon pool values the basic MEMC model configurations from Wang et al. 2013
#'
#' @format A vector of the intial states of the Carbon pools from MEND 2013 \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' \describe{ a named vector of the Carbon pools
#' \item{'P'}{'Particulate organic carbon or POC'}
#' \item{'M'}{'Mineral associated organic carbon or MOC, note that this excludes Q'}
#' \item{'Q'}{'Adsorbed phase of DOC'}
#' \item{'B'}{'Microbial biomass carbon or MBC'}
#' \item{'D'}{'Dissolved organic carbon or DOC'}
#' \item{'EP'}{'Enzyme for decomposition of P'}
#' \item{'EM'}{'Ensyme for decomposition of M'}
#' \item{'IC'}{'Accumulated CO2 flux'}
#' \item{'Tot'}{'Total carbon in the patch'}
#' }
#' @family inital values
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' default_initial
#' print(default_initial)
"default_initial"


#' MEMC model configurations for MEND Wang et al. 2013
#'
#'
#' @format an object created form \code{configure_model}.
#' \describe{
#' \item{name}{MEND}
#' \item{params}{the default MEMC parameter table}
#' \item{state}{the default inital state values for MEMC}
#' \item{carbon_pools_func}{carbon pools based on Wang et al. 2013}
#' \item{carbon_fluxes_func}{carbon fluxes based on Wang et al. 2013}
#' }
#' @family MEMC configurations
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' MEND_model
#' solve_model(mod = MEND_model, time = 1:10)
"MEND_model"

#' MEMC model configurations for MEND Wang et al. 2013
#'
#'
#' @format an object created form \code{configure_model}.
#' \describe{
#' \item{name}{COMISSION}
#' \item{params}{the default MEMC parameter table}
#' \item{state}{the default inital state values for MEMC}
#' \item{carbon_pools_func}{carbon pools based on Wang et al. 2013}
#' \item{carbon_fluxes_func}{TBD}
#' }
#' @family MEMC configurations
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @references \href{https://doi.org/10.1890/12-0681.1}{update with the comission documetation}
#' @examples
#' COMISSION_model
#' solve_model(mod = COMISSION_model, time = 1:10)
"COMISSION_model"
