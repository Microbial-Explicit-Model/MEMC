#' Define the carbon pool fluxes
#'
#' @param parms MEMC parameter table
#' @param DOMdecomp string indicator for type of dynamics used for the DOM decomposition
#' @param POMdecomp string indicator for type of dynamics used for the POM decomposition
#' @param MBdecay string indicator for type of dynamics used to model MB decay
#' @noRd
#' @family internal
carbon_fluxes_internal <- function(parms, DOMdecomp = "MM", POMdecomp = "MM", MBdecay = "DD"){

  assert_that(all(sapply(list(POMdecomp, DOMdecomp, MBdecay), is.character)))
  assert_that(sum(DOMdecomp %in% c("MM", "RMM", "ECA")) == 1, msg = 'DOMdecomp must be "MM", "RMM", "ECA"')
  assert_that(sum(POMdecomp %in% c("MM", "RMM", "ECA", "LM")) == 1, msg = 'POMdecomp must be "MM", "RMM", "ECA", "LM"')
  assert_that(sum(MBdecay %in% c("LM", "DD")) == 1, msg = 'MBdecay must be "LM" or "DD"')

  # Parse out the parameter values so they can be be used in the fluxes
  pars <- parms$value
  names(pars) <- parms$parameter

  with(as.list(pars),{

    # Create an empty to store all of the flux functions in
    fluxes <- list()

    if (DOMdecomp == "MM") {
      fluxes[["F1"]] = function(B, D){V.d * B * D / (K.d + D)}
    } else if (DOMdecomp == "RMM") {
      fluxes[["F1"]] = function(B, D){V.d * B * D / (K.d + B)}
    } else if (DOMdecomp == "ECA"){
      fluxes[["F1"]] = function(B, D){V.d * B * D / (K.d + D + B)}
    }

    if (POMdecomp == "MM") {
      fluxes[["F2"]] = function(EP, P){(V.p * EP * P) / (K.p + P)}
    } else if (POMdecomp == "RMM") {
      fluxes[["F2"]] = function(P, EP){(V.p * EP * P) / (K.p + EP)}
    } else if (POMdecomp == "ECA") {
      fluxes[["F2"]] = function(P, EP){(V.p * EP * P) / (K.p + P + EP)}
    } else if (POMdecomp == "LM") {
      fluxes[["F2"]] = function(P, EP) {V.p * P}
    }

    fluxes[["F3"]] = function(EM, M){(V.m * EM * M) / (K.m + M)}
    fluxes[["F6"]] = function(D, Q){ K.ads * D * (1 - Q / Q.max)}
    fluxes[["F7"]] = function(Q){K.des * Q / Q.max}

    if (MBdecay == "DD") {
      assert_that(dd.beta > 1)
      fluxes[["F8"]] = function(B){(1 - p.ep - p.em) * 0.4 * V.d * (B ^ dd.beta)}
    } else if (MBdecay == "LM"){
      assert_that(dd.beta == 1)
      fluxes[["F8"]] = function(B){(1 - p.ep - p.em) * 0.4 * V.d * (B ^ dd.beta)}
    }

    fluxes[["F9.ep"]] = function(B){p.ep * B * 0.4 * V.d}
    fluxes[["F9.em"]] = function(B){p.em * B * 0.4 * V.d}
    fluxes[["F10.ep"]] = function(EP){r.ep * EP}
    fluxes[["F10.em"]] = function(EM){r.em * EM}

    return(fluxes)

  })

}


#' Define the carbon pool model
#'
#' @param t numeric when to solve the model
#' @param state MEMC vector of the pool values
#' @param parms MEMC parameter table
#' @param DOMdecomp string indicator for type of dynamics used for the DOM decomposition
#' @param POMdecomp string indicator for type of dynamics used for the POM decomposition
#' @param MBdecay string indicator for type of dynamics used to model MB decay
#' @noRd
#' @family internal
carbon_pool_derivs <- function(t, state, parms, DOMdecomp, POMdecomp, MBdecay){

  pars <- parms$value
  names(pars) <- parms$parameter

  fluxes <- carbon_fluxes_internal(parms, DOMdecomp, POMdecomp, MBdecay)

  with(as.list(c(state, pars)),{

    F1 <- fluxes$F1(B, D)
    F2 <- fluxes$F2(EP, P)
    F3 <- fluxes$F3(EM, M)
    F6 <- fluxes$F6(D, Q)
    F7 <- fluxes$F7(Q)
    F8 <- fluxes$F8(B)
    F9.ep <- fluxes$F9.ep(B)
    F9.em <- fluxes$F9.em(B)
    F10.ep <- fluxes$F10.ep(EP)
    F10.em <- fluxes$F10.em(EM)

    # Define the system of differential equations to describes the
    # changes in the carbon pool states.
    # -----------------------------------------------------------
    # P = particulate organic carbon
    dP <- (1 - g.d) * F8 - F2 + Input
    # M = mineral-associated organic carbon (MOC)
    dM <- (1 - f.d) * F2 - F3 + Input
    # Q = active layer of MOC
    dQ <- F6 - F7
    # B = microbial biomass carbon
    dB <- F1 * CUE - F8 - (F9.ep + F9.em)
    # D = dissolved organic carbon
    dD <- f.d * F2 + g.d * F8 + F3 + (F10.em + F10.ep) - F1 - (F6 - F7)
    # EP = carbon stored as extra-cellular enzymes
    dEP <- F9.em - F10.ep
    # EM = carbon stored as extra-cellular enzymes
    dEM <- F9.em - F10.em
    # IC = inorganic carbon (CO2)
    dIC <- F1 * (1 - CUE)
    # Tot = the total carbon pool
    dTot <- -F1 * (1-CUE) + Input * 2

    # Return outputs
    return(list(c(dP, dM, dQ, dB, dD, dEP, dEM, dIC, dTot)))

  })


}


#' Internal solve model function
#'
#' @param mod object created from \code{configure_model}
#' @param time numeric vector of the time stamps of when to solve the model
#' @noRd
#' @family internal
sm_internal <- function(mod, time, ...){

  rslt <- deSolve::ode(y = mod[["state"]],
                       times = time,
                       func = carbon_pool_derivs,
                       parms = mod[["params"]],
                       DOMdecomp = mod[["table"]][["DOMdecomp"]],
                       POMdecomp = mod[["table"]][["POMdecomp"]],
                       MBdecay = mod[["table"]][["MBdecay"]])

  return(rslt)

}


#' Format the output into something that is nice to return to solve model
#'
#' @param rslt object returned from \code{sm_internal}
#' @param mod object created from \code{configure_model}
#' @noRd
#' @family internal
sm_format_out <- function(rslt, mod){

  # Now format the results into a nice data frame instead of a wide  matrix.
  out <- data.table::melt(data.table::as.data.table(rslt),
                          measure.vars = names(mod$state),
                          variable.name = "variable",
                          value.name = 'value')
  out$units <- 'mg C/g soil'

  if (is.null(mod$name)){
    name <- "(unnamed)"
  } else {
    name <- mod$name
  }
  out$name <- name

  return(out)

}

#' Solve a MEMC configuration
#'
#' @param mod model object
#' @param time a vector of the time steps
#' @param params default set to NULL, will then use the parameter table read in with the "mod" object.
#' @param state default set to NULL, will then use the state read read in with the "mod" object.
#' @param ... additional arguments that can be read into \code{deSolve::ode}
#' @return a long formatted data.table of the simulation results
#' @importFrom assertthat assert_that has_args
#' @importFrom deSolve ode
#' @export
#' @family helper
solve_model <- function(mod, time, params = NULL, state = NULL, ...){

  mod <- update_config(mod = mod, params = params, state = state)
  results <- sm_internal(mod = mod, time = time)
  out <- sm_format_out(rslt = results, mod = mod)

  return(list("model" = mod, "results" = out))

}
