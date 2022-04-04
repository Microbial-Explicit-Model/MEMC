
#' Make the objective function to use in \link[FME]{modFit}
#'
#' @param obs data.frame of the observations to fit the model to
#' @param config MEMC configuration
#' @return function to be used a the f argument in
#' @importFrom assertthat assert_that
#' @importFrom FME modCost
#' @export
make_objective_function <- function(obs, config){

  assert_that(is.data.frame(obs))
  assert_that(all(names(obs) %in% c("time", names(MEMC::default_initial))))
  assert_that(is_memc_config(config), msg = "obj is a model configuration created by configure_model")

  out <- function(p){
    table    <- update_params(p, config$params)
    out      <- solve_model(mod = config, time = unique(obs$time), params = config$table, state = config$state)
    out_wide <- data.table::dcast(out, formula = time ~ variable)
    return(modCost(model = out_wide, obs = obs))
  }

  return(out)

}
