#' Default parameter values used in MEMC model configurations
#'
#' Default parameter values used in MEMC model configurations,
#' from \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#'
#' @format A data table consisting of 4 columns with inputs
#' @section Columns:
#' \describe{
#' \item{parameter}{String character of the default MEND parameters.}
#' \item{description}{String character describing the parameter.}
#' \item{units}{String character of the parameter units.}
#' \item{value}{Numeric values taken from the table 2 of Wang et al. 2013}
#' }
#' @name default_params
#' @family parameters
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang, G., Post, W.M. and Mayes, M.A. (2013), Development of microbial-enzyme-mediated decomposition model parameters through steady-state and dynamic analyses. Ecological Applications, 23: 255-272. https://doi.org/10.1890/12-0681.1}
#' @usage data(default_params)
#' @keywords datasets
"default_params"

#' The initial carbon pool values used as the default for the MEMC model configurations
#'
#' The initial carbon pool values used as the default for the MEMC model configurations,
#' from \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#'
#' @format A vector of the initial states of the Carbon pools from  \href{doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
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
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang, G., Post, W.M. and Mayes, M.A. (2013), Development of microbial-enzyme-mediated decomposition model parameters through steady-state and dynamic analyses. Ecological Applications, 23: 255-272. https://doi.org/10.1890/12-0681.1}
#' @examples
#' default_initial
#' print(default_initial)
"default_initial"

#' MEMC MEND model configuration based on \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#'
#' @format an object created by \code{configure_model}.
#' \describe{
#' \item{name}{MEND}
#' \item{params}{the default MEMC parameter table}
#' \item{state}{the default inital state values for MEMC}
#' \item{carbon_pools_func}{carbon pools based on Wang et al. 2013}
#' \item{carbon_fluxes_func}{carbon fluxes based on Wang et al. 2013.}}
#' @family MEMC configurations
#' @name MEND_model
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang, G., Post, W.M. and Mayes, M.A. (2013), Development of microbial-enzyme-mediated decomposition model parameters through steady-state and dynamic analyses. Ecological Applications, 23: 255-272. https://doi.org/10.1890/12-0681.1}
#' @examples
#' MEND_model
#' solve_model(mod = MEND_model, time = 1:10)
"MEND_model"

#' MEMC COMISSION model configuration based on  \href{https://doi.org/10.1016/j.soilbio.2020.107912}{Ahrens et al. 2020}
#'
#' @format an object created by \code{configure_model}.
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
#' @references \href{https://doi.org/10.1016/j.soilbio.2020.107912}{Ahrens, Bernhard, Georg Guggenberger, Janet Rethemeyer, Stephan John, Bernd Marschner, Stefanie Heinze, Gerrit Angst, et al. 2020. “Combination of Energy Limitation and Sorption Capacity Explains 14C Depth Gradients.” Soil Biology & Biochemistry 148 (September): 107912.}
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang, G., Post, W.M. and Mayes, M.A. (2013), Development of microbial-enzyme-mediated decomposition model parameters through steady-state and dynamic analyses. Ecological Applications, 23: 255-272. https://doi.org/10.1890/12-0681.1}
#' @examples
#' COMISSION_model
#' solve_model(mod = COMISSION_model, time = 1:10)
"COMISSION_model"

#' MEMC CORPSE model configuration based on \href{https://doi.org/10.1038/nclimate2436}{Sulman et al. 2014}
#'
#' @format an object created by \code{configure_model}.
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
#' @references \href{doi:10.1038/nclimate2436}{Sulman, B. N., R. P. Phillips, A. C. Oishi, E. Shevliakova, and S. W. Pacala (2014), Microbe-driven turnover offsets mineral-mediated storage of soil carbon under elevated CO2, Nat. Clim. Change, 4(12), 1099–1102, doi:10.1038/nclimate2436.}
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang, G., Post, W.M. and Mayes, M.A. (2013), Development of microbial-enzyme-mediated decomposition model parameters through steady-state and dynamic analyses. Ecological Applications, 23: 255-272. https://doi.org/10.1890/12-0681.1}
#' @examples
#' CORPSE_model
#' solve_model(mod = CORPSE_model, time = 1:10)
"CORPSE_model"
