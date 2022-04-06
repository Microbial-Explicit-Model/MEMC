
#' Make the objective function to use in \link{MEMC_modFit}
#'
#' @param obs data.frame of the user provided observations
#' @param config MEMC configuration
#' @return function to be used a the f argument in FME::modFit
#' @importFrom assertthat assert_that
#' @importFrom FME modCost
#' @noRd
make_objective_function <- function(obs, config){

  assert_that(is.data.frame(obs))
  assert_that(all(names(obs) %in% c("time", names(MEMC::default_initial))))
  assert_that(is_memc_config(config), msg = "obj is a model configuration created by configure_model")

  out <- function(p){
    table    <- update_params(p, config$params)
    out <- sm_internal(mod = config, time = unique(obs$time), params = table, state = config$state)
    return(modCost(model = out, obs = obs))
  }

  return(out)

}

#' Constrain a MEMC model configuration to user provided data
#'
#' @param obs data.frame of the user provided observations
#' @param config MEMC configuration
#' @param p vector of the parameters to be constrained while fitting a MEMC model to user provided data
#' @param ... additional arguments passed to function f (modFit)
#' @return object returned from FME::modFit
#' @importFrom assertthat assert_that
#' @importFrom FME modFit
#' @export
MEMC_modFit <- function(obs, config, p, ...){

  # Check inputs
  assert_that(all(names(p) %in% config$params$parameter))
  assert_that(all(is.numeric(p)))

  # Make the objective function to minimize and get the model fit.
  obj_fxn <- make_objective_function(obs, config)
  out <- modFit(f = obj_fxn, p = p, ...)

  return(out)
}
