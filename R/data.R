#' The parameter values for MEND 2013
#'
#' @docType data
#' @usage data(MEND_params)
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
#' @family MEND
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
"MEND_params"


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
#' @family inital values
#' @family MEND
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
"MEND_initalState"


#' The parameter values for the 2 pool MEND
#'
#' @format A data table consisting of 4 columns with inputs from \href{TODO}{TODO}.
#' \describe{
#' \item{parameter}{String character of the default 2 pool MEND parameters.}
#' \item{description}{String character describing the parameter.}
#' \item{units}{String character of the parameter units.}
#' \item{value}{Numeric values taken from TODO}
#' }
#' @family parameters
#' @family MEND2
#' @references \href{TODO}{TODO}
"MEND2_params"


#' The inital carbon pool values for MEND 2013
#'
#' @format A vector of the intial states of the Carbon pools from MEND 2013 \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' \describe{ a named vecotr of the MEND 2013 Carbon pools
#' \item{'P1'}{'First pool of particulate organic carbon or POC'}
#' \item{'P2'}{'Second pool of particulate organic carbon or POC'}
#' \item{'M'}{'Mineral associated organic carbon or MOC, note that this excludes Q'}
#' \item{'Q1'}{'Adsorbed phase from DOC pool 1'}
#' \item{'Q2'}{'Adsorbed phase from DOC pool 2'}
#' \item{'B'}{'Microbial biomass carbon or MBC'}
#' \item{'D1'}{'Dissolved organic carbon or DOC from pool 1'}
#' \item{'D2'}{'Dissolved organic carbon or DOC from pool 2'}
#' \item{'EP1'}{'Enzyme for decomposition of P1'}
#' \item{'EP2'}{'Enzyme for decomposition of P2'}
#' \item{'EM'}{'Ensyme for decomposition of M'}
#' \item{'IC'}{'Accumulated CO2 flux'}
#' \item{'Tot'}{'Total carbon in the patch'}
#' }
#' @family inital values
#' @family MEND2
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
"MEND2_initalState"

