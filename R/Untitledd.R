#'
#' library(deSolve)
#'
#' # the challange is going to be some how setting up th the derivs in a way so that the if statement is not being read every
#' # single time but also felxible enough to modify the derivs and make our pretty table....
#'
#'
#' # challange 1 set up setting
#' # old new with jz output
#' # testing the derivs, sm_internal, sm_format_out,  solve_model
#' # dev the ability to change params
#' # test ability to change params
#' #... phase 2 will be doing the differet configurations!
#'
#'
#'
#' # Something that is not clear yet is how to do this with other
#' derivs <- function(t, state, parms){
#'
#'   pars <- parms$value
#'   names(pars) <- parms$parameter
#'
#'
#'   with(as.list(c(state, pars)),{
#'
#'     F1 <- V.d * B * D / (K.d + D)             # DOC uptake by microbial biomass.
#'     F2 <- (V.p * EP * P) / (K.p + P)
#'     F3 <- (V.m * EM * M) / (K.m + M)
#'     F6 <- K.ads * D * (1 - Q / Q.max)
#'     F7 <- K.des * Q / Q.max
#'     F8 <- (1 - p.ep - p.em) * B * 0.4 * V.d * B
#'     F9.ep <- p.ep * B * 0.4 * V.d
#'     F9.em <- p.em * B * 0.4 * V.d
#'     F10.ep <- r.ep * EP
#'     F10.em <- r.em * EM
#'
#'
#'     # Define the system of differential equations to describes the
#'     # changes in the carbon pool states.
#'     # -----------------------------------------------------------
#'     # P = particulate organic carbon
#'     dP <- (1 - g.d) * F8 - F2 + Input
#'     # M = mineral-associated organic carbon (MOC)
#'     dM <- (1 - f.d) * F2 - F3 + Input
#'     # Q = active layer of MOC
#'     dQ <- F6 - F7
#'     # B = microbial biomass carbon
#'     dB <- F1 * CUE - F8 - (F9.ep + F9.em)
#'     # D = dissolved organic carbon
#'     dD <- f.d * F2 + g.d * F8 + F3 + (F10.em + F10.ep) - F1 - (F6 - F7)
#'     # EP = carbon stored as extra-cellular enzymes
#'     dEP <- F9.em - F10.ep
#'     # EM = carbon stored as extra-cellular enzymes
#'     dEM <- F9.em - F10.em
#'     # IC = inorganic carbon (CO2)
#'     dIC <- F1 * (1 - CUE)
#'     # Tot = the total carbon pool
#'     dTot <- -F1 * (1-CUE) + Input * 2
#'
#'     # Return outputs
#'     list(c(dP, dM, dQ, dB, dD, dEP, dEM, dIC, dTot))
#'
#'   })
#'
#'
#' }
#'
#' # need to set up a way so that new parameters & state values can be read into...
#' sm_internal <- function(mod, time, params = NULL, state = NULL, ...){
#'
#'   inital_vals <- mod[["state"]]
#'   ptable <- mod[["params"]]
#'
#'   rslt  <- ode(y = inital_vals,
#'                times = time,
#'                func = mod[["derivs"]],
#'                parms = ptable,
#'                ...)
#'
#'   return(rslt)
#'
#' }
#'
#' sm_format_out <- function(rslt, mod){
#'
#'   # Now format the results into a nice data frame instead of a wide  matrix.
#'   out <- data.table::melt(data.table::as.data.table(rslt),
#'                           measure.vars = names(mod$state),
#'                           variable.name = "variable",
#'                           value.name = 'value')
#'   out$units <- 'mg C/g soil'
#'
#'   if (is.null(mod$name)){
#'     name <- "(unnamed)"
#'   } else {
#'     name <- mod$name
#'   }
#'   out$name <- name
#'
#'   return(out)
#'
#' }
#'
#' #' Solve a MEMC configuration
#' #'
#' #' @param mod model object
#' #' @param time a vector of the time steps
#' #' @param params default set to NULL, will then use the parameter table read in with the "mod" object.
#' #' @param state default set to NULL, will then use the state read read in with the "mod" object.
#' #' @param ... additional arguments that can be read into \code{deSolve::ode}
#' #' @return a long formatted data.table of the simulation results
#' #' @importFrom assertthat assert_that has_args
#' #' @importFrom deSolve ode
#' #' @export
#' #' @family helper function
#' solve_model <- function(mod, time, params = NULL, state = NULL, ...){
#'
#'   results <- sm_internal(mod = mod, time = time, params = params, state = state)
#'   out <- sm_format_out(rslt = results, mod = mod)
#'
#'   return(out)
#' }
#'
#'
#' name <- "test"
#' params <- default_params
#' state <- default_initial
#'
#' config <- list("name" = name,
#'                "table" = table,
#'                "params" = params,
#'                "state" = state,
#'                "derivs" = derivs)
#'
#'
#' x <- sm_internal(config, times)
#' xx <- sm_format_out(x, mod)
#'
#' yy <- solve_model(config, times)
#'
#'
