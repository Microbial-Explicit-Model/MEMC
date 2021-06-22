#' Define 2 carbon pool MEND
#'
#' \code{MEND_2pools} Defines the system of equations that
#' describe the state of the carbon pools from \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013} modified
#' modified with 2 carbon pools.
#'
#' @param t is for time
#' @param state A numeric vector of the different MEND carbon pool states.
#' @param parms A data frame of the parameters.
#' @param flux_function a function that will return a list of functions that modify how carbon moves between
#' the different carbon pools.
#' @return A list of the state variables
#' @importFrom Rdpack reprompt
#' @importFrom assertthat assert_that has_name
#' @family MEND2
#' @family carbon pool functions
#' @noRd
MEND2_pools <- function(t, state, parms, flux_function = MEND2_fluxes){

  # Check the inputs
  required_states <- c("P1", "P2", "M", "B", "D1", "D2", "Q1", "Q2", "EP1", "EP2", "EM", "IC", "Tot")
  missing_states  <- required_states[!required_states %in% names(state)]
  assert_that(length(missing_states) == 0, msg = paste0('missing states: ', paste(missing_states, collapse = ',  ')))
  assert_that(all(required_states  == names(state)), msg = paste0('state pools must be in the following order: ', paste(required_states, collapse = ',  ')))
  assert_that(has_name(x = parms, which = c("parameter", "description", "units", "value")))
  assert_that(is.function(flux_function))

  req <- c('I.p1', 'I.p2', 'g.d', 'f.d', 'I.d1', 'I.d2')
  missing <- setdiff(req, parms$parameter)
  assert_that(length(missing) == 0, msg = paste('parms missing values for: ', paste0(missing, collapse = ', ')))


  # Format the parameters into a vector.
  p        <- parms$value
  names(p) <- parms$parameter

  with(as.list(c(state, p)),{

    # Define the fluxes and check to make sure they meet the requirements to be used
    # by the MEND carbon pool structure.
    fluxes <- flux_function(state = state, parms = parms)

    expected_fluxes <- c("F1d1", "F1d2", "F2p1", "F2p2", "F3", "F6d1", "F6d2", "F7d1",
                         "F7d2", "F8", "F9.ep1", "F9.ep2", "F9.em", "F10.ep1", "F10.ep2", "F10.em" )
    assertthat::assert_that(assertthat::has_name(x = fluxes, which = expected_fluxes))
    assertthat::assert_that(all(unlist(lapply(fluxes, is.function))), msg = 'fluxes input must be a list of functions')

    # Define the system of differental equations to describes the
    # changes in the carbon pool states.

    # -----------------------------------------------------------
    # P = particulate organic carbon (2 pools)
    dP1 <- I.p1 + (1 - g.d) * fluxes$F8() - fluxes$F2p1()
    dP2 <- I.p2 - fluxes$F2p2()
    # M = mineral-associated organic carbon (MOC)
    dM <- (1 - f.d) * (fluxes$F2p1() + fluxes$F2p2()) - fluxes$F3()
    # Q = active layer of MOC (2 pools)
    dQ1 <- fluxes$F6d1() - fluxes$F7d1()
    dQ2 <- fluxes$F6d2() - fluxes$F7d2()
    # B = microbial biomass carbon
    dB <- E.c1*fluxes$F1d1() + E.c2*fluxes$F1d2() -  fluxes$F8() - (fluxes$F9.em() + fluxes$F9.ep1() +  fluxes$F9.ep2())
    # D = dissolved organic carbon (2 pools)
    dD1 <- I.d1 +  g.d * fluxes$F8() +  fluxes$F3()  + 0.5 * f.d * (fluxes$F2p1() + fluxes$F2p2()) + (fluxes$F10.em() + fluxes$F10.ep1() + + fluxes$F10.ep2()) - fluxes$F1d1() - (fluxes$F6d1() - fluxes$F7d1())
    dD2 <- I.d2 +  0.5 * f.d*  (fluxes$F2p1() + fluxes$F2p2()) - fluxes$F1d2() - (fluxes$F6d2() - fluxes$F7d2())
    # EP = carbon stored as extracellular enzymes (2 pools)
    dEP1 <- fluxes$F9.ep1() - fluxes$F10.ep1()
    dEP2 <- fluxes$F9.ep2() - fluxes$F10.ep2()
    # EM = carbon stored as extracellular enzymes
    dEM <- fluxes$F9.em() - fluxes$F10.em()
    # IC = inorganic carbon (CO2)
    dIC <-  (1 - E.c1) *  fluxes$F1d1() + (1 - E.c2) *  fluxes$F1d2()
    # Tot = the total carbon pool
    dTot <- I.p1 + I.p2 + I.d1 + I.d2 - (1 - E.c1)*fluxes$F1d1() - (1 - E.c2)*fluxes$F1d2()

    # Return outputs
    list(c(dP1, dP2, dM, dB, dD1, dD2, dQ1, dQ2, dEP1, dEP2, dEM, dIC, dTot))
  })
}




#' Define the 2 P pool MEND fluxes
#'
#' \code{MEND2_fluxes} Defines a system of equations that
#' describe the state of the fluxes between the pools from the 2 pool version of
#' \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}. By retruning a list of named functions.
#'
#' @param state A numeric vector of the different MEND carbon pool states.
#' @param parms A data frame of the parameters.
#' @return A list of functions that calculate the fluxes between the MEND 2 carbon pools.
#' @family MEND2
#' @family carbon flux functions
#' @importFrom assertthat assert_that has_name
#' @noRd
MEND2_fluxes <- function(state, parms){

  # Check inputs
  assert_that(has_name(x = state, which = c("P1", "P2", "M", "B", "D1", "D2", "Q1",
                                            "Q2", "EP1", "EP2", "EM", "IC", "Tot")))
  assert_that(has_name(x = parms, which = c("parameter", "description", "units", "value")))
  assert_that(has_name(x = parms, which = c("parameter",  "value")))

  req <- c('E.c1', 'E.c2', 'V.d1', 'V.d2', 'K.d1', 'K.d2', 'V.p1', 'V.p2', 'K.p1', 'K.p2', 'K.m', 'K.ads',
           'Q.max1', 'Q.max2', 'K.des', 'p.ep', 'p.em', 'r.ep',  'r.em')
  missing <- setdiff(req, parms$parameter)
  assert_that(length(missing) == 0, msg = paste('parms missing values for: ', paste0(missing, collapse = ', ')))


  # Format the parameters into a vector.
  p        <- parms$value
  names(p) <- parms$parameter

  with(as.list(c(state, p)), {

    fxn_list <- list(
      "F1d1" = function(){
        # Microbial uptake from D1 pool
        V.d1 * B * D1 /( K.d1 + D1)
      },
      "F1d2" = function(){
        # Microbial uptake from D2 pool
        V.d2 * B * D2 /( K.d2 + D2)
      },
      "F2p1" = function(){
        # Decomposition of P1 pool
        V.p1 * EP1 * P1 / (K.p1 + P1)
      },
      "F2p2" = function(){
        # Decomposition of P2 pool
        V.p2 * EP2 * P2 / (K.p2 + P2)
      },
      "F3" = function(){
        # Break down of mineralized organic carbon
        V.m * EM * M / (K.m + M)
      },
      "F6d1" = function(){
        # Adsorption of DOC 1 pool to mineral-associated organic carbon
        K.ads * D1 *(1- Q1/ Q.max1)
      },
      "F6d2" = function(){
        # Adsorption of DOC 2 pool to mineral-associated organic carbon
        K.ads * D2 *(1- Q2/ Q.max2)
      },
      "F7d1" = function(){
        # Desorption of mineral-associated organic carbon to DOC pool1
        K.des * Q1/Q.max1
      },
      "F7d2" = function(){
        # Desorption of mineral-associated organic carbon to DOC pool 2
        K.des * Q2/Q.max2
      },
      "F8" = function(){
        # Carbon loss due to microbial biomass mortality
        (1 - p.ep - p.ep - p.em) * V.d1 * E.c1 * B * 0.3
      },
      "F9.ep1" = function(){
        # Enzyme production
        P1 / (P1+P2) * p.ep * V.d1 * E.c1 * B * 0.2
      },
      "F9.ep2" = function(){
        # Enzyme production
        P2 / (P1+P2) * p.ep * V.d1 * E.c1 * B * 0.2
      },
      "F9.em" = function(){
        # Enzyme production
        p.em * V.d1 * E.c1 * B * 0.2
        },
      "F10.ep1" = function(){
        # Enzyme turn over
        r.ep * EP1 },
      "F10.ep2" = function(){
        # Enzyme turn over
        r.ep * EP2 },
      "F10.em" = function(){

        # Enzyme turn over
        r.em * EM })

    return(fxn_list)

  })

}


#' Solve 2 P pool MEND
#'
#' \code{MEND2} Run and solve MEND 2013 \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' modified with two different pools.
#'
#' @param parameters data.table containing the following columns: parameter, value, and units. Default values are stored as pacakge see \code{MEND2_params}.
#' @param time a vector of the time setps.
#' @param inital_state a numeric vector of the different MEND carbon pool states. Default values are probvide as package data see \code{MEND2_initalState}
#' @param method optional string argument that can be passed in as the method into the \code{ode}.
#' @return a data frame of MEND output variables
#' @family MEND2
#' @family models
#' @examples
#' # define the time vector & load the default parameters and initial state values stored
#' # as package data.
#' t <- seq(0, 175200, 24) ##per hour 24hour by 365 days =8760  20year=175200
#' p <- MEND2_params
#' state <- MEND2_initalState
#'
#' # Solve MEND2
#' out <- MEND2(parameters = p, time = t,  inital_state = state)
#' @export
MEND2 <- function(parameters, time, inital_state, method = NULL){

  out <- solver(params = parameters,
                time = time,
                state = inital_state,
                carbon_pools_func = MEND2_pools,
                carbon_fluxes_func = MEND2_fluxes, method = method)

  return(out)

}






