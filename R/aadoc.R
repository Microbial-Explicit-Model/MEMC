#### Documentation objects

#' Configurations
#'
#' The various model configurations that are
#' shipped with the MEMC package, such as:
#' \code{MEND_model}, \code{COMISSION_model}, \code{CORPSE_model},
#' \code{MIMCS_model}, and \code{MEMS_model}.
#'
#' These model configurations can be used with the \code{solve_model} function to
#' complete the desired model simulations.
#' @name configurations
#' @seealso dynamics
#'
#' @examples
#' ## Using solve_model with the MEND_model configuration.
#' solve_model(mode = MEND_model, time = 0:10)
NULL


#' Identifiers for MEMC supported flux representations
#'
#' These identifiers correspond to the various dynamics users can select
#'  \code{\link{configure_model}}
#'
#' @section Dynamics:
#' \describe{
#' \item{MM}{Michaelis–Menten kineticsf}
#' \item{RMM}{Reverse Michaelis-Menten kinetics, as in (Schimel and Weintraub, 2003) and (Sulman et al. 2014)}
#' \item{ECA}{Equilibrium chemistry approximation, based on RESOM (Tang and Riley, 2014)}
#' \item{DD}{Density dependent microbial decay}
#' \item{LM}{Linear model, as in (Robertson et al. 2019)}
#' }
#' @name dynamics
#' @references \href{doi:10.1016/s0038-0717(03)00015-4}{Schimel, J. P., and M. N. Weintraub (2003), The implications of exoenzyme activity on microbial carbon and nitrogen limitation in soil: A theoretical model, Soil Biol. Biochem., 35(4), 549–563, doi:10.1016/s0038-0717(03)00015-4}
#' @references \href{doi:10.1038/nclimate2436}{Sulman, B. N., R. P. Phillips, A. C. Oishi, E. Shevliakova, and S. W. Pacala (2014), Microbe-driven turnover offsets mineral-mediated storage of soil carbon under elevated CO2, Nat. Clim. Change, 4(12), 1099–1102, doi:10.1038/nclimate2436.}
#' @references \href{https://doi.org/10.1038/nclimate2438}{Tang, J., Riley, W. Weaker soil carbon–climate feedbacks resulting from microbial and abiotic interactions. Nature Clim Change 5, 56–60 (2015). https://doi.org/10.1038/nclimate2438}
#' @references \href{https://doi.org/10.5194/bg-16-1225-2019}{Robertson, A. D., Paustian, K., Ogle, S., Wallenstein, M. D., Lugato, E., and Cotrufo, M. F.: Unifying soil organic matter formation and persistence frameworks: the MEMS model, Biogeosciences, 16, 1225–1248, https://doi.org/10.5194/bg-16-1225-2019, 2019.}
#' @seealso configurations
NULL


