#' The parameter values for the basic MEMC model configurations from Wang et al. 2013
#'
#' @docType data
#' @usage data(memc_params)
#' @keywords datasets
#'
#' @format A data frame of 20 rows and 4 columns containing the model
#' parameter values from \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' \describe{
#' \item{parameter}{String character of the default MEND parameters.}
#' \item{description}{String character describing the parameter.}
#' \item{units}{String character of the parameter units.}
#' \item{value}{Numeric values taken from the table 2 of Wang et al. 2013}
#' }
#' @family inputs
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' print(memc_params)
"memc_params"


#' Soil incubation respiration fluxes from Wang et al. 2013
#'
#' @docType data
#' @usage data(memc_data_ultisol)
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
#' memc_data_ultisol
"memc_data_ultisol"


#' The default initial carbon pool values used by the default MEMC model configurations
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
#' print(default_initial)
"default_initial"


#' The MEMC model configuration for MEND
#'
#' The MEND configuration is based off of Wang et al. 2013. This
#' configuration uses MM for DOM uptake, MM for POM decomposition,
#' and LM for MB decay; see \link{dynamics} for more details.
#'
#' @format An object created from \code{\link{memc_configure}}.
#' \describe{
#' \item{name}{MEND}
#' \item{table}{table of the dynamics used by this model configuration; see \link{dynamics} for more details.}
#' \item{params}{data table of the model parameters; see \link{memc_params} for more details.}
#' \item{state}{vector of the initial state values; see \link{default_initial} for more details.}
#' }
#' @family configurations
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' ## Use memc_solve to run the simulation using this configuration.
#' print(MEND_model)
#' memc_solve(mod = MEND_model, time = 1:10)
"MEND_model"


#' The MEMC model configuration for COMISSION
#'
#' The COMISSION configuration is based off of Ahrens et al. 2015. The
#' model uses MM for the DOM uptake, RMM for POM decomposition, and LM
#' for MB decay; see \link{dynamics} for more details.
#'
#' @format An object created from \code{\link{memc_configure}}, containing the following elements.
#' \describe{
#' \item{name}{COMISSION}
#' \item{table}{table of the dynamics used by this model configuration; see \link{dynamics} for more details.}
#' \item{params}{data table of the model parameters; see \link{memc_params} for more details.}
#' \item{state}{vector of the initial state values; see \link{default_initial} for more details.}
#' }
#' @family configurations
#' @references \href{https://doi.org/10.1016/j.soilbio.2015.06.008}{Ahrens et al. 2015}
#' @examples
#' ## Use memc_solve to run the simulation using this configuration.
#' print(COMISSION_model)
#' memc_solve(mod = COMISSION_model, time = 1:10)
"COMISSION_model"


#' The MEMC model configuration for CORPSE
#'
#' The CORPSE configuration is based off of Sulman et al. 2018. This
#' model uses RMM for DOM uptake, LM for POM decomposition, and LM for
#' MB decay; see \link{dynamics} for more details.
#'
#' @format An object created from \code{\link{memc_configure}}, containing the following elements.
#' \describe{
#' \item{name}{CORPSE}
#' \item{table}{table of the dynamics used by this model configuration; see \link{dynamics} for more details.}
#' \item{params}{data table of the model parameters; see \link{memc_params} for more details.}
#' \item{state}{vector of the initial state values; see \link{default_initial} for more details.}
#' }
#' @family configurations
#' @references \href{https://doi.org/10.1007/s10533-018-0509-z}{Sulman et al. 2018}
#' @examples
#' ## Use memc_solve to run the simulation using this configuration.
#' print(CORPSE_model)
#' memc_solve(mod = CORPSE_model, time = 1:10)
"CORPSE_model"


#' The MEMC model configuration for MIMCS
#'
#' The MIMCS configuration is based off of Wieder et al. 2015. This
#' model uses MM for the DOM uptake, MM for POM decomposition, and DD
#' for MB decay; see \link{dynamics} for more details.
#'
#' @format An object created from \code{\link{memc_configure}}, containing the following elements.
#' \describe{
#' \item{name}{MIMCS}
#' \item{table}{table of the dynamics used by this model configuration; see \link{dynamics} for more details.}
#' \item{params}{data table of the model parameters; see \link{memc_params} for more details.}
#' \item{state}{vector of the initial state values; see \link{default_initial} for more details.}
#' }
#' @family configurations
#' @references \href{https://doi.org/10.1002/2015GB005188}{Wieder et al. 2015}
#' @examples
#' ## Use memc_solve to run the simulation using this configuration.
#' print(MIMCS_model)
#' memc_solve(mod = MIMCS_model, time = 1:10)
"MIMCS_model"


#' The MEMC model configuration for MEMS
#'
#' The MEMS configuration is based off of Robertson et al. 2019. This
#' model uses LM for the DOM uptake, LM for POM decomposition, and LM
#' for MB decay; see \link{dynamics} for more details.
#'
#' @format An object created from \code{\link{memc_configure}}, containing the following elements.
#' \describe{
#' \item{name}{MEMS}
#' \item{table}{table of the dynamics used by this model configuration; see \link{dynamics} for more details.}
#' \item{params}{data table of the model parameters; see \link{memc_params} for more details.}
#' \item{state}{vector of the initial state values; see \link{default_initial} for more details.}
#' }
#' @family configurations
#' @references \href{https://doi.org/10.5194/bg-16-1225-2019}{Robertson et al. 2019}
#' @examples
#' ## Use memc_solve to run the simulation using this configuration.
#' print(MEMS_model)
#' memc_solve(mod = MEMS_model, time = 1:10)
"MEMS_model"


#' The MEMC model configuration for BAMS
#'
#' The BAMS configuration is based off of Tang et al. 2022. This model
#' uses MM for the DOM uptake, MM for POM decomposition, and LM for MB
#' decay; see \link{dynamics} for more details.
#'
#' @format An object created from \code{\link{memc_configure}}, containing the following elements.
#' \describe{
#' \item{name}{BAMS}
#' \item{table}{table of the dynamics used by this model configuration; see \link{dynamics} for more details.}
#' \item{params}{data table of the model parameters; see \link{memc_params} for more details.}
#' \item{state}{vector of the initial state values; see \link{default_initial} for more details.}
#' }
#' @family configurations
#' @references \href{https://doi.org/10.5194/bg-16-1225-2019}{Robertson et al. 2019}
#' @examples
#' ## Use memc_solve to run the simulation using this configuration.
#' print(BAMS_model)
#' memc_solve(mod = MEMS_model, time = 1:10)
"BAMS_model"


#' List of all the pre-defined MEMC models
#'
#' @format A easy-to-iterate-over list, each element of which is one of the MEMC models.
#' @seealso \code{\link{MEND_model}}, \code{\link{COMISSION_model}},
#' \code{\link{CORPSE_model}}, \code{\link{MEMS_model}},
#' \code{\link{BAMS_model}}, \code{\link{MIMCS_model}}
#' @examples
#' print(memc_all_models)
"memc_all_models"

#' Summary data frame of all the pre-defined MEMC model configurations
#'
#' @format data frame containing 4 columns
#' \describe{
#' \item{model}{model configuration name}
#' \item{DOMuptake}{DOM uptake by microbial biomass; see \link{dynamics} for more details.}
#' \item{POMdecomp}{POM decomposition; see \link{dynamics} for more details.}
#' \item{MBdecay}{microbial biomass decay; see \link{default_initial} for more details.}
#' }
#' @family configurations
#' @examples
#' print(model_configs)
"model_configs"
