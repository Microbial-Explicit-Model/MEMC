#' The parameter values for MEND 2013
#'
#' @format A data table consisting of 4 columns with inputs from \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' \describe{
#' \item{parameter}{String character of the default MEND parameters.}
#' \item{description}{String character describing the parameter.}
#' \item{units}{String character of the parameter units.}
#' \item{value}{Numeric values taken from the table 2 of Wang et al. 2013}
#' }
#' @family parameters
#' @family MEND2012
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
"MEND2013_params"


#' The inital carbon pool values for MEND 2013
#'
#' @format A vector of the intial states of the Carbon pools from MEND 2013 \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' \describe{ a named vecotr of the MEND 2013 Carbon pools
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
#' @family parameters
#' @family MEND2013
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
"MEND2013_initalState"

