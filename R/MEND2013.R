# This script defines the 2013 MEND version of the model.


#' Define 2013 MEND carbon pools
#'
#' \code{MEND2013_pools} Defines the system of equations that
#' describe the state of the carbon pools from \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#'
#' @param t is for time
#' @param state A numeric vector of the different MEND carbon pool states.
#' @param parms A data frame of the parameters.
#' @param flux_function a function that will return a list of functions that modify how carbon moves between
#' the different MEND carbon pools the default is setup to reproduce the structure documented in
#' \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' @return A list of the state variables
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @importFrom Rdpack reprompt
#' @importFrom assertthat assert_that has_name
#' @family 2013 MEND model functions
#' @family carbon pool functions
#' @noRd
MEND2013_pools <- function(t, state, parms, flux_function = MEND2013_fluxes){

  # Check the inputs
  required_states <- c("P", "M", "Q", "B", "D", "EP", "EM", "IC", "Tot")
  missing_states  <- required_states[!required_states %in% names(state)]
  assert_that(length(missing_states) == 0, msg = paste0('missing states: ', paste(missing_states, collapse = ',  ')))
  assert_that(all(required_states  == names(state)), msg = paste0('state pools must be in the following order: ', paste(required_states, collapse = ',  ')))
  assert_that(data.table::is.data.table(parms))
  assert_that(has_name(x = parms, which = c("parameter", "description", "units", "value")))
  assert_that(is.function(flux_function))

  req <- c('I.p', 'g.d', 'f.d', 'I.p', 'I.d')
  missing <- setdiff(req, parms$parameter)
  assert_that(length(missing) == 0, msg = paste('parms missing values for: ', paste0(missing, collapse = ', ')))


  # Format the parameters into a vector.
  p        <- parms$value
  names(p) <- parms$parameter

  with(as.list(c(state, p)),{

    # Define the fluxes and check to make sure they meet the requirements to be used
    # by the MEND carbon pool structure.
    fluxes <- flux_function(state = state, parms = parms)

    expected_fluxes <- rep('F', length.out = length(1:8))
    expected_fluxes <- c(paste0(expected_fluxes, 1:8), 'F9.ep', 'F9.em', 'F10.ep', 'F10.em')
    assertthat::assert_that(assertthat::has_name(x = fluxes, which = expected_fluxes))
    assertthat::assert_that(all(unlist(lapply(fluxes, is.function))), msg = 'fluxes input must be a list of functions')

    # Define the system of differental equations to describes the
    # changes in the carbon pool states.
    # -----------------------------------------------------------
    # P = particulate organic carbon
    dP <- I.p + (1 - g.d) * fluxes$F8() - fluxes$F2()
    # M = mineral-associated organic carbon (MOC)
    dM <- (1 - f.d) * fluxes$F2() - fluxes$F3()
    # Q = active layer of MOC
    dQ <- fluxes$F6() - fluxes$F7()
    # B = microbial biomass carbon
    dB <- fluxes$F1() - (fluxes$F4() + fluxes$F5()) - fluxes$F8() - (fluxes$F9.ep() + fluxes$F9.em())
    # D = dissolved organic carbon
    dD <- I.d + f.d * fluxes$F2() + g.d * fluxes$F8() + fluxes$F3() + (fluxes$F10.em() + fluxes$F10.ep()) - fluxes$F1() - (fluxes$F6() - fluxes$F7())
    # EP = carbon stored as extracellular enzymes
    dEP <- fluxes$F9.em() - fluxes$F10.ep()
    # EM = carbon stored as extracellular enzymes
    dEM <- fluxes$F9.em() - fluxes$F10.em()
    # IC = inorganic carbon (CO2)
    dIC <- fluxes$F4() + fluxes$F5()
    # Tot = the total carbon pool
    dTot <- I.p + I.d - (fluxes$F4() + fluxes$F5())

    # Return outputs
    list(c(dP, dM, dQ, dB, dD, dEP, dEM, dIC, dTot))
  })
}



#' Define the 2013 MEND fluxes.
#'
#' \code{MEND2013_fluxes} Defines a system of equations that
#' describe the state of the fluxes between the pools from \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' By retruning a list of named functions.
#'
#' @param state A numeric vector of the different MEND carbon pool states.
#' @param parms A data frame of the parameters.
#' @return A list of functions that calculate the fluxes between \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013} carbon pools.
#' @family 2013 MEND model functions
#' @family carbon flux functions
#' @importFrom assertthat assert_that has_name
#' @noRd
MEND2013_fluxes <- function(state, parms){

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
        # DOC uptake by microbial biomass.
        (1/E.c) * (V.d + m.r) * B *D /( K.d + D)
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


#' Solve MEND 2013
#'
#' \code{MEND2013} Run and solve MEND 2013 \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#'
#' @param parameters data.table containing the following columns: parameter, value, and units. Default values are stored as pacakge see \code{MEND2013_params}.
#' @param time a vector of the time setps.
#' @param inital_state a numeric vector of the different MEND carbon pool states. Default values are probvide as package data see \code{MEND2013_initalState}
#' @return a data frame of MEND output variables
#' @family 2013 MEND model functions
#' @family model
#' @export
MEND2013 <- function(parameters, time, inital_state){


  out <- solver(params = parameters,
                time = time,
                state = inital_state,
                carbon_pools_func = MEND2013_pools,
                carbon_fluxes_func = MEND2013_fluxes)

  return(out)

}

