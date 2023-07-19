#' The parameter values for the basic MEMC model configurations from Wang et al. 2013
#'
#' @docType data
#' @usage data(default_params)
#' @keywords datasets
#'
#' @format A data frame of 20 rows and 4 columns containing the model parameter values from \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' \describe{
#' \item{parameter}{String character of the default MEND parameters.}
#' \item{description}{String character describing the parameter.}
#' \item{units}{String character of the parameter units.}
#' \item{value}{Numeric values taken from the table 2 of Wang et al. 2013}
#' }
#' @family inputs
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' print(default_params)
"default_params"


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
#' The MEND configuration is based off of Wang et al. 2013, this
#' configuration uses MM for DOM uptake, MM for
#' POM decomposition, and a LM for MB decay, see \link{dynamics} for more
#' details.
#'
#' @format an object created form \code{configure_model}.
#' \describe{
#' \item{name}{MEND}
#' \item{table}{table of the dynamics used by this model configuration, see \link{dynamics} for more details.}
#' \item{params}{data table of the model parameters, see \link{default_params} for more details.}
#' \item{state}{vector of the initial state values, see \link{default_initial} for more details.}
#' }
#' @family {configurations}
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @examples
#' ## Use solve_model to run the simulation using this configuration.
#' print(MEND_model)
#' solve_model(mod = MEND_model, time = 1:10)
"MEND_model"

#' The MEMC model configuration for COMISSION
#'
#' The COMISSION configuration is based off of Ahrens et al. 2015, the model uses MM for the
#' DOM uptake, RMM for POM decomposition, and a LM for MB decay, see \link{dynamics} for more
#' details.
#'
#' @format an object created form \code{configure_model}, containing the following elements.
#' \describe{
#' \item{name}{COMISSION}
#' \item{table}{table of the dynamics used by this model configuration, see \link{dynamics} for more details.}
#' \item{params}{data table of the model parameters, see \link{default_params} for more details.}
#' \item{state}{vector of the initial state values, see \link{default_initial} for more details.}
#' }
#' @family {configurations}
#' @references \href{https://doi.org/10.1016/j.soilbio.2015.06.008}{Ahrens et al. 2015}
#' @examples
#' ## Use solve_model to run the simulation using this configuration.
#' print(COMISSION_model)
#' solve_model(mod = COMISSION_model, time = 1:10)
"COMISSION_model"

#' The MEMC model configuration for CORPSE
#'
#' The CORPSE configuration is based off of Sulman et al. 2018, this model uses RMM for
#' DOM uptake, LM for POM decomposition, and a LM for MB decay, see \link{dynamics} for more
#' details.
#'
#' @format an object created form \code{configure_model}, containing the following elements.
#' \describe{
#' \item{name}{CORPSE}
#' \item{table}{table of the dynamics used by this model configuration, see \link{dynamics} for more details.}
#' \item{params}{data table of the model parameters, see \link{default_params} for more details.}
#' \item{state}{vector of the initial state values, see \link{default_initial} for more details.}
#' }
#' @family {configurations}
#' @references \href{https://doi.org/10.1007/s10533-018-0509-z}{Sulman et al. 2018}
#' @examples
#' ## Use solve_model to run the simulation using this configuration.
#' print(CORPSE_model)
#' solve_model(mod = CORPSE_model, time = 1:10)
"CORPSE_model"

#' The MEMC model configuration for MIMCS
#'
#' The MIMCS configuration is based off of Wieder et al. 2015, this model uses MM for the DOM
#' uptake, MM for POM decomposition, and a DD for MB decay, see \link{dynamics} for more
#' details.
#'
#' @format an object created form \code{configure_model}, containing the following elements.
#' \describe{
#' \item{name}{MIMCS}
#' \item{table}{table of the dynamics used by this model configuration, see \link{dynamics} for more details.}
#' \item{params}{data table of the model parameters, see \link{default_params} for more details.}
#' \item{state}{vector of the initial state values, see \link{default_initial} for more details.}
#' }
#' @family {configurations}
#' @references \href{https://doi.org/10.1002/2015GB005188}{Wieder et al. 2015}
#' @examples
#' ## Use solve_model to run the simulation using this configuration.
#' print(MIMCS_model)
#' solve_model(mod = MIMCS_model, time = 1:10)
"MIMCS_model"

#' The MEMC model configuration for MEMS
#'
#' The MEMS configuration is based off of Robertson et al. 2019, this model uses LM for the DOM
#' uptake, LM for POM decomposition, and a LM for MB decay, see \link{dynamics} for more
#' details.
#'
#' @format an object created form \code{configure_model}, containing the following elements.
#' \describe{
#' \item{name}{MEMS}
#' \item{table}{table of the dynamics used by this model configuration, see \link{dynamics} for more details.}
#' \item{params}{data table of the model parameters, see \link{default_params} for more details.}
#' \item{state}{vector of the initial state values, see \link{default_initial} for more details.}
#' }
#' @family {configurations}
#' @references \href{https://doi.org/10.5194/bg-16-1225-2019}{Robertson et al. 2019}
#' @examples
#' ## Use solve_model to run the simulation using this configuration.
#' print(MEMS_model)
#' solve_model(mod = MEMS_model, time = 1:10)
"MEMS_model"

#' The MEMC model configuration for BAMS
#'
#' The BAMS configuration is based off of Tang et al. 2022, this model uses MM for the DOM
#' uptake, MM for POM decomposition, and a LM for MB decay, see \link{dynamics} for more
#' details.
#'
#' @format an object created form \code{configure_model}, containing the following elements.
#' \describe{
#' \item{name}{BAMS}
#' \item{table}{table of the dynamics used by this model configuration, see \link{dynamics} for more details.}
#' \item{params}{data table of the model parameters, see \link{default_params} for more details.}
#' \item{state}{vector of the initial state values, see \link{default_initial} for more details.}
#' }
#' @family {configurations}
#' @references \href{https://doi.org/10.5194/bg-16-1225-2019}{Robertson et al. 2019}
#' @examples
#' ## Use solve_model to run the simulation using this configuration.
#' print(BAMS_model)
#' solve_model(mod = MEMS_model, time = 1:10)
"BAMS_model"




#' Data table of all the pre-defined MEMC model configurations
#'
#'
#' @format data table containing 4 columns
#' \describe{
#' \item{model}{model configuration name}
#' \item{DOMuptake}{DOM uptake by microbial biomass, see \link{dynamics} for more details.}
#' \item{POMdecomp}{POM decomposition, see \link{dynamics} for more details.}
#' \item{MBdecay}{microbial biomass decay, see \link{default_initial} for more details.}
#' }
#' @family {configurations}
#' @examples
#' print(model_configs)
"model_configs"




#' Color palette for the MEMC model configurations
#'
#'
#' @format vector defining the color palette to use for plotting results
#' @family {configurations}
#' @references \href{https://doi.org/10.5194/bg-16-1225-2019}{Robertson et al. 2019}
#' @examples
#' print(model_configs)
"model_configs"
