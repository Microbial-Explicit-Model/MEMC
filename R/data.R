#' Parameter values for the MEMC model configuration
#'
#' @docType data
#' @usage data(memc_params)
#' @keywords datasets
#'
#' @format A data frame of 20 rows and 4 columns containing the default MEMC
#' parameter values
#' \describe{
#' \item{parameter}{String indicating MEMC parameter.}
#' \item{description}{String describing the parameter.}
#' \item{units}{String of the parameter units.}
#' \item{value}{Parameter values}
#' }
#' @family inputs
#' @references Wang et al.: Development of microbial-enzyme-mediated
#' decomposition model parameters through steady-state and dynamic analyses,
#' Ecol. Appl., 23, 255–272, 2013. \url{https://doi.org/10.1890/12-0681.1}
#' @examples
#' memc_params
"memc_params"


#' Soil incubation respiration fluxes from Wang et al. 2013
#'
#' @docType data
#' @usage data(memc_incubation_ultisol)
#' @keywords datasets
#' @format A list containing:
#' \describe{
#' \item{data}{A data frame with 45 rows (15 time points x three replicates) and 3 columns:
#' \describe{
#' \item{Day}{Day of incubation.}
#' \item{Soil}{String constant describing soil type.}
#' \item{value}{Cumulative respiration, mg C/g soil.}
#' }}
#' \item{state}{A named vector of the initial states of the carbon pools associated with the observations}
#' }
#' @references Wang et al.: Development of microbial-enzyme-mediated
#' decomposition model parameters through steady-state and dynamic analyses,
#' Ecol. Appl., 23, 255–272, 2013. \url{https://doi.org/10.1890/12-0681.1}
#' @examples
#' memc_incubation_ultisol
"memc_incubation_ultisol"


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
#' @references Wang et al.: Development of microbial-enzyme-mediated
#' decomposition model parameters through steady-state and dynamic analyses,
#' Ecol. Appl., 23, 255–272, 2013. \url{https://doi.org/10.1890/12-0681.1}
#' @examples
#' print(memc_initial_state)
"memc_initial_state"


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
#' \item{state}{vector of the initial state values; see \link{memc_initial_state} for more details.}
#' }
#' @family configurations
#' @references Wang et al.: Development of microbial-enzyme-mediated
#' decomposition model parameters through steady-state and dynamic analyses,
#' Ecol. Appl., 23, 255–272, 2013. \url{https://doi.org/10.1890/12-0681.1}
#' @examples
#' ## Use memc_solve to run the simulation using this configuration.
#' print(MEND_config)
#' memc_solve(mod = MEND_config, time = 1:10)
"MEND_config"


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
#' \item{state}{vector of the initial state values; see \link{memc_initial_state} for more details.}
#' }
#' @family configurations
#' @references Ahrens et al.: Contribution of sorption, DOC transport and
#' microbial interactions to the 14C age of a soil organic carbon profile:
#' Insights from a calibrated process model, Soil Biol. Biochem., 88, 390–402,
#' 2015. \url{https://doi.org/10.1016/j.soilbio.2015.06.008}
#' @examples
#' ## Use memc_solve to run the simulation using this configuration.
#' print(COMISSION_config)
#' memc_solve(mod = COMISSION_config, time = 1:10)
"COMISSION_config"


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
#' \item{state}{vector of the initial state values; see \link{memc_initial_state} for more details.}
#' }
#' @family configurations
#' @references Sulman et al.: Multiple models and experiments underscore large
#' uncertainty in soil carbon dynamics, Biogeochemistry, 141, 109–123, 2018.
#' \url{https://doi.org/10.1007/s10533-018-0509-z}
#' @examples
#' ## Use memc_solve to run the simulation using this configuration.
#' print(CORPSE_config)
#' memc_solve(mod = CORPSE_config, time = 1:10)
"CORPSE_config"


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
#' \item{state}{vector of the initial state values; see \link{memc_initial_state} for more details.}
#' }
#' @family configurations
#' @references Wieder, W. R., et al.: Explicitly representing soil microbial
#' processes in Earth system models, Global Biogeochem. Cycles, 29, 1782–1800,
#' 2015. \url{https://doi.org/10.1002/2015GB005188}
#' @examples
#' ## Use memc_solve to run the simulation using this configuration.
#' print(MIMCS_config)
#' memc_solve(mod = MIMCS_config, time = 1:10)
"MIMCS_config"


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
#' \item{state}{vector of the initial state values; see \link{memc_initial_state} for more details.}
#' }
#' @family configurations
#' @references Robertson et al.: Unifying soil organic matter formation and
#' persistence frameworks: the MEMS model, Biogeosciences, 16, 1225–1248, 2019.
#' \url{https://doi.org/10.5194/bg-16-1225-2019}
#' @examples
#' ## Use memc_solve to run the simulation using this configuration.
#' print(MEMS_config)
#' memc_solve(mod = MEMS_config, time = 1:10)
"MEMS_config"


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
#' \item{state}{vector of the initial state values; see \link{memc_initial_state} for more details.}
#' }
#' @family configurations
#' @references Robertson et al.: Unifying soil organic matter formation and
#' persistence frameworks: the MEMS model, Biogeosciences, 16, 1225–1248, 2019.
#' \url{https://doi.org/10.5194/bg-16-1225-2019}
#' @examples
#' ## Use memc_solve to run the simulation using this configuration.
#' print(BAMS_config)
#' memc_solve(mod = MEMS_config, time = 1:10)
"BAMS_config"


#' List of all the pre-defined MEMC models
#'
#' @format A easy-to-iterate-over list, each element of which is one of the MEMC models.
#' @seealso \code{\link{MEND_config}}, \code{\link{COMISSION_config}},
#' \code{\link{CORPSE_config}}, \code{\link{MEMS_config}},
#' \code{\link{BAMS_config}}, \code{\link{MIMCS_config}}
#' @examples
#' memc_all_configs
#'
#' # Return the summary table for dynamics used in model configurations
#' summary(memc_all_configs)
"memc_all_configs"


#' Example data, time series and initial conditions for Ultisol from Wang et al. 2013 (10.1890/12-0681.1)
#'
#' A list containing incubation data and initial states.
#'
#' @format A list of 2 elements:
#' \describe{
#'   \item{\code{data}}{data frame of  time-series data for carbon pools from the incubation experiment.
#'     The data frame contains the following variables:
#'     \describe{
#'       \item{\code{Day}}{numeric, indicating the time since the start of the incubation experiment.}
#'       \item{\code{Soil}}{Character indicating the soil type (e.g., "Ultisol").}
#'       \item{\code{IC}}{Numeric value for inorganic carbon (mg C g\eqn{^{-1}} soil).}
#'     }
#'   }
#'   \item{\code{state}}{named numeric vector of initial state values for the soil carbon pools.}
#' }
#'
#' @details The `data` element contains time-series observations, while the `state` element represents the initial conditions for the soil carbon pools.
#'
#' @examples
#' # Access the incubation data
#' memc_incubation_ultisol$data
#'
#' # Access the initial state
#' memc_incubation_ultisol$state
"memc_incubation_ultisol"


#' Example data, time series and initial conditions for Andisol from Wang et al. 2013 (10.1890/12-0681.1)
#'
#' A list containing incubation data and initial states.
#'
#' @format A list of 2 elements:
#' \describe{
#'   \item{\code{data}}{data frame of  time-series data for carbon pools from the incubation experiment.
#'     The data frame contains the following variables:
#'     \describe{
#'       \item{\code{Day}}{numeric, indicating the time since the start of the incubation experiment.}
#'       \item{\code{Soil}}{Character indicating the soil type (e.g., "Andisol").}
#'       \item{\code{IC}}{Numeric value for inorganic carbon (mg C g\eqn{^{-1}} soil).}
#'     }
#'   }
#'   \item{\code{state}}{named numeric vector of initial state values for the soil carbon pools.}
#' }
#'
#' @details The `data` element contains time-series observations, while the `state` element represents the initial conditions for the soil carbon pools.
#'
#' @examples
#' # Access the incubation data
#' memc_incubation_andisol$data
#'
#' # Access the initial state
#' memc_incubation_andisol$state
"memc_incubation_andisol"


#' Example data, time series and initial conditions for Gelisol from Wang et al. 2013 (10.1890/12-0681.1)
#'
#' A list containing incubation data and initial states.
#'
#' @format A list of 2 elements:
#' \describe{
#'   \item{\code{data}}{data frame of  time-series data for carbon pools from the incubation experiment.
#'     The data frame contains the following variables:
#'     \describe{
#'       \item{\code{Day}}{numeric, indicating the time since the start of the incubation experiment.}
#'       \item{\code{Soil}}{Character indicating the soil type (e.g., "Gelisol").}
#'       \item{\code{IC}}{Numeric value for inorganic carbon (mg C g\eqn{^{-1}} soil).}
#'     }
#'   }
#'   \item{\code{state}}{named numeric vector of initial state values for the soil carbon pools.}
#' }
#'
#' @details The `data` element contains time-series observations, while the `state` element represents the initial conditions for the soil carbon pools.
#'
#' @examples
#' # Access the incubation data
#' memc_incubation_gelisol$data
#'
#' # Access the initial state
#' memc_incubation_gelisol$state
"memc_incubation_gelisol"


#' Example data, time series and initial conditions for Mollisol from Wang et al. 2013 (10.1890/12-0681.1)
#'
#' A list containing incubation data and initial states.
#'
#' @format A list of 2 elements:
#' \describe{
#'   \item{\code{data}}{data frame of  time-series data for carbon pools from the incubation experiment.
#'     The data frame contains the following variables:
#'     \describe{
#'       \item{\code{Day}}{numeric, indicating the time since the start of the incubation experiment.}
#'       \item{\code{Soil}}{Character indicating the soil type (e.g., "Mollisol").}
#'       \item{\code{IC}}{Numeric value for inorganic carbon (mg C g\eqn{^{-1}} soil).}
#'     }
#'   }
#'   \item{\code{state}}{named numeric vector of initial state values for the soil carbon pools.}
#' }
#'
#' @details The `data` element contains time-series observations, while the `state` element represents the initial conditions for the soil carbon pools.
#'
#' @examples
#' # Access the incubation data
#' memc_incubation_mollisol$data
#'
#' # Access the initial state
#' memc_incubation_mollisol$state
"memc_incubation_mollisol"
