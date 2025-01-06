#' Soil incubation respiration fluxes from Wang et al. 2013
#'
#' @docType data
#' @usage data(memc_data_andisol)
#' @keywords datasets
#' @format A data frame of 45 rows (15 time points x three replicates)
#' and 3 columns containing respiration data.
#' \describe{
#' \item{Day}{Day of incubation.}
#' \item{Soil}{String constant describing soil type.}
#' \item{value}{Cumulative respiration, mg C/g soil.}
#' }
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' memc_data_andisol
"memc_data_andisol"

#' @docType data
#' @rdname memc_data_andisol
"memc_data_gelisol"

#' @docType data
#' @rdname memc_data_andisol
"memc_data_mollisol"

#' @docType data
#' @rdname memc_data_andisol
"memc_data_oxisol"

#' @docType data
#' @rdname memc_data_andisol
"memc_data_ultisol"


#' Initial carbon pool values used by the default MEMC model configurations
#'
#' @format A named vector of the initial states of the carbon pools.
#' \describe{ a named vector of the Carbon pools
#' \item{POM}{Particulate organic carbon or POC}
#' \item{MOM}{Mineral associated organic carbon or MOC, note that this excludes QOM}
#' \item{QOM}{Adsorbed phase of DOC}
#' \item{MB}{Microbial biomass carbon or MBC}
#' \item{DOM}{Dissolved organic carbon or DOC}
#' \item{EP}{Enzyme for decomposition of POM}
#' \item{EM}{Enzyme for decomposition of MOM}
#' \item{IC}{Accumulated CO2 flux}
#' \item{Tot}{Total carbon in the patch}
#' }
#' @family inputs
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' print(memc_state_andisol)
"memc_state_andisol"

#' @docType data
#' @rdname memc_state_andisol
"memc_state_gelisol"

#' @docType data
#' @rdname memc_state_andisol
"memc_state_mollisol"

#' @docType data
#' @rdname memc_state_andisol
"memc_state_ultisol"

