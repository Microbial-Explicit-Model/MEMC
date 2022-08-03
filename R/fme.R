#' Make the objective function
#'
#' @param comp_data data frame to be used as the comparison data
#' @param x vector of the parameters or state values to be used
#' @param config memc model configuration
#' @return function to  be used in the FME::modFit
#' @importFrom assertthat assert_that has_args
#' @noRd
#' @family fme
make_memc_objective <- function(comp_data, x, config) {
  assert_that("time" %in% names(comp_data), msg = "comp_data must contain time column")
  comp_data_vars <- names(comp_data)[names(comp_data) != "time"]
  assert_that(comp_data_vars %in% names(MEMC::default_initial), msg = "comp_data must contain a MEMC variable")

  fxn <- function(x) {
    # split up up the input into model parameters and state value
    xx <- split_param_state(x)
    new_p <- xx$params
    new_s <- xx$state

    # the time steps to evaluate the model at
    t <- seq(0, max(comp_data$time))

    # solve the model
    new_config <-
      update_config(mod = config,
                    params = new_p,
                    state = new_s)
    out <- sm_internal(new_config, t)

    # make sure that the model solved for all time steps
    model_time <- unique(out['time'])
    run_complete <- all(t %in% model_time)
    assert_that(!run_complete, msg = "model run terminated early")
    # to do there should be some better way to handle this... ideally with the
    # TODO  add something that makes a fake output table? to use int he model cost?
    # Limit model output to only the time steps of the comparison data.
    #out <- out[out['time'] %in% comp_data$time, ]

    # calculate the model cost using the FME modCost function
    return(FME::modCost(model = out, obs = comp_data))

  }

  return(fxn)

}


#' Fit a MEMC model to a comparison data
#'
#' @param config memc model configuration
#' @param x vector of the parameters or state values to be fit using FME::modFit
#' @param comp_data data frame containing the comparison to fit the data to
#' @param lower lower bounds on the parameters; if unbounded set equal to -Inf
#' @param upper bounds on the parameters; if unbounded set equal to Inf
#' @param ... addition arguments that may be passed to FME::modFit
#' @return the results of the FME::modFit
#' @export
#' @family fme
memc_modfit <-
  function(config,
           x,
           comp_data,
           lower = -Inf,
           upper = Inf,
           ...) {
    obj <-
      make_memc_objective(config = config,
                          x = x,
                          comp_data = comp_data)
    out <- FME::modFit(
      p = x,
      f = obj,
      lower = lower,
      upper = upper
    )

    return(out)

  }

#
# #sensRange(func = solveLIN, parms = pars, dist ="norm", parMean=mean, parCovar=covar, sensvar = c("DOM", "POM","MOM","MB"), parRange = parRanges, num = 100))
#
# memc_sensrange <- function(config, t){
#
#   fxn <- function(parms){
#
#     # split up up the input into model parameters and state value
#     xx <- split_param_state(parms)
#     new_p <- xx$params
#     new_s <- xx$state
#
#     new_config <- update_config(mod = config, params = new_p, state = new_s)
#     out <- sm_internal(new_config, t)
#
#     return(out)
#
#   }
#
#
#
#   fxn(x = c("V.d" =  9))
#
#   FME::sensRange(func = fxn, parms = , dist = dist,)
#
#
# }
