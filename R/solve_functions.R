#' Solve a MEMC configuration
#'
#' @param mod model object created by \code{make_model}
#' @param time a vector of the time steps
#' @param params default set to NULL, will then use the parameter table read in with the "mod" object.
#' @param state default set to NULL, will then use the state read read in with the "mod" object.
#' @param ... additional arguments that can be read into \code{deSolve::ode}
#' @return a long formatted data.table of the simulation results
#' @importFrom assertthat assert_that has_args
#' @importFrom deSolve ode
#' @export
#' @family helper function
solve_model <- function(mod, time, params = NULL, state = NULL, ...){

  # Check the function arguments
  assert_that(sm_internal_check_args(mod = mod, time = time, params = params, state = state))

  results <- sm_internal(mod = mod, time = time, params = params, state = state)

  out <- sm_format_out(rslt = results, mod = mod)

  return(out)
}



#' Solve a MEMC configuration
#'
#' @param mod model object created by \code{make_model}
#' @param time a vector of the time steps
#' @param params default set to NULL, will then use the parameter table read in with the "mod" object.
#' @param state default set to NULL, will then use the state read read in with the "mod" object.
#' @return TRUE if the arguements pass all the checks
#' @importFrom assertthat assert_that has_args
#' @noRd
#' @family solve model internal
sm_internal_check_args <- function(mod, time, params = NULL, state = NULL){

  assert_that(is_memc_config(mod), msg = "mod must a model object created by configure_model")
  assert_that((is.null(params) | is.data.frame(params)))
  assert_that((is.null(state) | is.vector(state)))
  assert_that(all(is.numeric(time)))

  return(TRUE)


}


#' Solve a MEMC configuration
#'
#' @param mod model object created by \code{make_model}
#' @param time a vector of the time steps
#' @param params default set to NULL, will then use the parameter table read in with the "mod" object.
#' @param state default set to NULL, will then use the state read read in with the "mod" object.
#' @param ... additional arguments that can be read into \code{deSolve::ode}
#' @return a long formatted data.table of the simulation results
#' @importFrom assertthat assert_that has_args
#' @importFrom deSolve ode
#' @export
#' @family helper functions
sm_internal <- function(mod, time, params = NULL, state = NULL, ...){

  env <- mod[["env"]]

  # Update model parameter & initial state values if needed.
  if(!is.null(params)){
    p <- params$value
    names(p) <- params$parameter
    modify_env(env, p)
  } else if (is.null(params)){
    params <- mod[["params"]]
  }
  if(!is.null(state)){
    modify_env(env, state)
  } else if (is.null(state)){
    state <- mod[['state']]
  }

  # Solve the model
  c_pool <- mod$carbon_pools_func
  c_flux <- mod$carbon_fluxes_func

  rslt  <- ode(y = state,
               times = time,
               func = c_pool,
               flux_function = c_flux,
               env = env,
               parms = params,
               ...)

  return(rslt)

}

#' Solve a MEMC configuration
#'
#' @param rslt deSolve matrix
#' @param mod model object created by \code{make_model}
#' @return a long formatted data.table of the simulation results
#' @export
#' @family helper functions
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

