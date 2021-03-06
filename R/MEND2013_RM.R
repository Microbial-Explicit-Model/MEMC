#' Define MEND fluxes that use reverse michaelis menten kinetics
#'
#' \code{MEND2013_RM_fluxes} Defines a system of equations that
#' describe the state of the fluxes between the pools from \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' By retruning a list of named functions. This set of functions differs from \code{MEND2013_fluxes} in its
#' represenation of the DOC uptake by microbial biomass. Here this dynamic is driven by reverse michaelis menten kinetics.
#'
#' @param state A numeric vector of the different MEND carbon pool states.
#' @param parms A data frame of the parameters.
#' @return A list of functions that calculate the fluxes between \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013} carbon pools.
#' @family 2013 MEND model functions
#' @family carbon flux functions
#' @importFrom assertthat assert_that has_name
#' @noRd
MEND2013_RM_fluxes <- function(state, parms){

  # Check inputs
  assert_that(has_name(x = state, which = c("B", "D", "P", "Q", "M", "EP", "EM")))
  assert_that(data.table::is.data.table(parms))
  assert_that(has_name(x = parms, which = c("parameter", "description", "units", "value")))
  req <- c('E.c', 'V.d', 'm.r', 'K.d', 'V.p', 'K.p', 'V.m', 'K.m', 'K.ads',
           'Q.max', 'K.des', 'p.ep', 'p.em', 'r.ep',  'r.em')
  missing <- setdiff(req, parms$parameter)
  assert_that(length(missing) == 0, msg = paste('parms missing values for: ', paste0(missing, collapse = ', ')))


  # Format the parameters into a vector.
  p        <- parms$value
  names(p) <- parms$parameter

  with(as.list(c(state, p)), {

    fxn_list <- list(
      "F1" = function(){
        # DOC uptake by microbial biomass, note that this uses reverse michaelis menten kinetics.
        (1/E.c) * (V.d + m.r) * B * D /(K.d + B)
      },
      "F2" = function(){
        # POC decomposition
        V.p * EP * P / (K.p + P)
      },
      "F3" = function(){
        # Break down of mineralized organic carbon
        V.m * EM * M / (K.m + M)
      },
      "F4" = function(){
        # Microbial respiration from biomass growth
        (1/E.c -1) * V.d * B * D /( K.d + D)
      },
      "F5" = function(){
        # Metabolic/maintenance microbial respiration
        (1/E.c -1) * m.r * B * D /( K.d + D)
      },
      "F6" = function(){
        # Adsorption of DOC to mineral-associated organic carbon
        K.ads * D *(1- Q/ Q.max)
      },
      "F7" = function(){
        # Desorption of mineral-associated organic carbon to DOC
        K.des * Q/Q.max
      },
      "F8" = function(){
        # Carbon loss due to microbial biomass mortality
        (1 - p.ep - p.em) * m.r * B
      },
      "F9.ep" = function(){
        # Enzyme production
        p.ep * m.r *B
      },
      "F9.em" = function(){
        # Enzyme production
        p.em * m.r * B },
      "F10.ep" = function(){
        # Enzyme turn over
        r.ep * EP },
      "F10.em" = function(){
        # Enzyme turn over
        r.em * EM })

    return(fxn_list)

  })

}


#' Solve MEND 2013 with reverse michaelis menten kinetics
#'
#' \code{MEND2013_RM} Run and solve MEND 2013  \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013} that
#' uses reverse michaelis menten kinetics in microbial uptake of DOC.
#'
#' @param parameters data.table containing the following columns: parameter, value, and units. Default values are stored as pacakge see \code{MEND2013_params}.
#' @param time a vector of the time setps.
#' @param inital_state a numeric vector of the different MEND carbon pool states. Default values are probvide as package data see \code{MEND2013_initalState}
#' @return a data frame of MEND output variables
#' @family 2013 MEND model functions
#' @family model
#' @export
MEND2013_RM <- function(parameters, time, inital_state){


  out <- solver(params = parameters,
                time = time,
                state = inital_state,
                carbon_pools_func = MEND2013_pools,
                carbon_fluxes_func = MEND2013_RM_fluxes)

  return(out)

}
