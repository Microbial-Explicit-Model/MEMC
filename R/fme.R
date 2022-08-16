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


#' Run the memc model with the FME sensRange
#'
#' @param config memc model configuration
#' @param t vector of the time steps to run the model at
#' @param pars vector of the parameters to test in sensRange
#' @param parRange data frame of the min/max parameter values
#' @param dist str for the distribution according to which the parameters will be sampled
#' @param ... additional arguments passed to FME::sensRange
#' @return the results of the FME::sensRange
#' @export
#' @family fme
#'@examples
#'\dontrun{
#' # Test the sensitivity of the MEND output for V.p, K.p, V.m, K.m, V.d, K.d
#' pars <- c("V.d" = 3.0e+00,"V.p" = 1.4e+01,"V.m" = 2.5e-01)
#' prange <- data.frame(min = pars - pars * 0.75,
#' max = pars + pars * 0.75)
#' t <- floor(seq(0, 365, length.out = 10))
#' out <- memc_sensrange(config = MEND_model, t = t, pars = pars, parRange = prange, dist = "latin", num = 10)
#' plot(summary(out))
#'}
memc_sensrange <- function(config, t, pars, parRange, dist, ...){

  func <- function(pars){
    new_mod <- update_config(mod = config, params = pars)
    sm_internal(mod = new_mod, time = t)

  }

  out <- FME::sensRange(func, parms = pars, dist = dist, parRange = parRange, ...)

  return(out)
}


#' Format the memc_sensrange output into long data format for easy plotting
#'
#' @param obj object returned by memc_sensrange
#' @return the a long dataframe of the summary memc_sensrange
#' @export
#'@examples
#'\dontrun{
#' # Test the sensitivity of the MEND output for V.p, K.p, V.m, K.m, V.d, K.d
#' pars <- c("V.d" = 3.0e+00,"V.p" = 1.4e+01,"V.m" = 2.5e-01)
#' prange <- data.frame(min = pars - pars * 0.75,
#' max = pars + pars * 0.75)
#' t <- floor(seq(0, 365, length.out = 10))
#' out <- memc_sensrange(config = MEND_model, t = t, pars = pars, parRange = prange, dist = "latin", num = 10)
#' to_plot <- format_sensrange(out)
#'}
format_sensrange <- function(obj){

  assert_that(class(obj)[[1]] == "sensRange")
  out <- summary(obj)
  names(out)[1] <- "time"
  vars <- gsub(pattern = "\\.|[[:digit:]]+", replacement = "", x = row.names(out))
  out$variable <- vars
  row.names(out) <- NULL

  return(out)

}
