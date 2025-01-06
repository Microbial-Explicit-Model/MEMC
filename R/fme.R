#' Make the objective function
#'
#' @param comp_data data frame to be used as the comparison data
#' @param x vector of the parameters or state values to be used
#' @param config MEMC model configuration
#' @return Function to  be used in the \code{FME}::\code{\link[FME]{modFit}}.
#' @importFrom assertthat assert_that has_args
#' @noRd
#' @family fme
make_memc_objective <- function(comp_data, x, config) {

  assert_that("time" %in% names(comp_data), msg = "comp_data must contain time column")
  comp_data_vars <- names(comp_data)[names(comp_data) != "time"]
  assert_that(all(comp_data_vars %in% names(MEMC::memc_state_ultisol)),
              msg = "comp_data must contain a MEMC variable")

  fxn <- function(x) {

    # the time steps to evaluate the model at
    t <- seq(0, max(comp_data$time))

    # solve the model
    new_config <- update_config(mod = config, new = x)
    out <- sm_internal(new_config, t)

    # make sure that the model solved for all time steps
    model_time <- unique(out['time'])
    run_complete <- all(t %in% model_time)
    assert_that(!run_complete, msg = "model run terminated early")
    # to do there should be some better way to handle this...
    # TODO  add something that makes a fake output table? to use in the model cost?
    # Limit model output to only the time steps of the comparison data.
    #out <- out[out['time'] %in% comp_data$time, ]

    # calculate the model cost using the FME modCost function
    return(FME::modCost(model = out, obs = comp_data))

  }

  return(fxn)

}


#' Fit a MEMC model to a comparison data
#'
#' @param x MEMC model parameters or initial conditions that will be fit to the data, users will need to provide an initial guess for these values.
#' @param config MEMC model configuration object, either one of the pre-built configurations listed in \code{model_configs} or created using \code{memc_configure}
#' @param comp_data data frame containing the comparison data that the model will
#'  be fit this data frame must contain a column for time, the other columns must be named for the MEMC model variables.
#' @param lower lower bounds on the parameters; if unbounded set equal to -Inf
#' @param upper bounds on the parameters; if unbounded set equal to Inf
#' @param ... addition arguments that may be passed to FME::modFit
#' @return Results from \code{FME}::\code{\link[FME]{modFit}}.
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


#' Global sensitivity ranges for a MEMC model
#'
#' Given a MEM model configuration, estimate the global parameter sensitivity.
#'
#' @param config a MEMC model configuration object, either one of the pre-built configurations listed in \code{model_configs} or created using \code{memc_configure}
#' @param t vector of the time steps to run the model at
#' @param x vector of the parameters or initial model pool sizes that will be varied
#' @param parRange data frame of the min/max parameter values
#' @param dist str for the distribution according to which the parameters will be sampled from, options" "unif" (uniformly random samples), "norm", (normally distributed random samples), "latin" (latin hypercube distribution), and "grid" (parameters arranged on a grid).
#' @param ... additional arguments passed to \code{\link[FME]{sensRange}}
#' @return The results of \code{FME}::\code{\link[FME]{sensRange}}.
#' @export
#' @family sensitivity
#' @examples
#'\dontrun{
#' # Test the sensitivity of the MEND output for V_p, K_p, V_m
#' pars <- c("V_d" = 3.0e+00,"V_p" = 1.4e+01,"V_m" = 2.5e-01)
#' prange <- data.frame(min = pars - pars * 0.75,
#' max = pars + pars * 0.75)
#' t <- floor(seq(0, 365, length.out = 10))
#' out <- memc_sensrange(config = MEND_model, t = t, x = pars,
#' parRange = prange, dist = "latin", num = 10)
#' plot(summary(out))
#' # Using the helper functions.
#' to_plot <- memc_format_sensout(out)
#' ggplot(data = to_plot) +
#'    geom_line(aes(time, Mean)) +
#'    geom_ribbon(aes(time, ymin = Min, ymax = Max), alpha = 0.5) +
#'    facet_wrap("variable", scales = "free")
#'}
memc_sensrange <- function(config, t, x, parRange, dist, ...){

  func <- function(pars){
    new_mod <- update_config(mod = config, new = pars)
    sm_internal(mod = new_mod, time = t)
  }

  out <- FME::sensRange(func, parms = x, dist = dist, parRange = parRange, ...)

  return(out)
}


#' Local sensitivity for a MEMC model
#'
#' Estimate the local effect of a parameter on a MEMC model output
#'
#' @param config a MEMC model configuration object, either one of the pre-built configurations listed in \code{model_configs} or created using \code{memc_configure}
#' @param t vector of the time steps to run the model at
#' @param x vector of the parameters or initial state values to test
#' @param ... additional arguments passed to \code{\link[FME]{sensFun}}
#' @return The results of \code{FME}::\code{\link[FME]{sensFun}}.
#' @export
#' @family sensitivity
#' @examples
#'\dontrun{
#' # Test the sensitivity of the MEND output for V_p, K_p, V_m
#' pars <- c("V_d" = 3.0e+00,"V_p" = 1.4e+01,"V_m" = 2.5e-01)
#' out <- memc_sensfunc(config = MEND_model, t = t, x = pars)
#' pairs(out)
#' plot(out)
#' # Using the helper functions to make nice ggplots
#' to_plot <- memc_format_sensout(out)
#' ggplot(data = to_plot) +
#'    geom_line(aes(time, value, color = parameter)) +
#'    facet_wrap("variable", scales = "free")
#'}
memc_sensfunc <- function(config, t, x, ...){

  func <- function(x){
    new_mod <- update_config(mod = config, new = x)
    sm_internal(mod = new_mod, time = t)
  }

  out <- FME::sensFun(func, x, ...)

}


#' Format the output returned by memc_sensrange or memc_sensfunc
#' into long data format for easy plotting
#'
#' @param obj object returned by memc_sensrange
#' @return A long-format data frame containing the summary
#' from \code{FME}::\code{\link[FME]{sensRange}}.
#' @export
#' @family sensitivity
#' @examples
#'\dontrun{
#' # Test the sensitivity of the MEND output for V_p, K_p, V_m, K_m, V_d, K_d
#' pars <- c("V_d" = 3.0e+00,"V_p" = 1.4e+01,"V_m" = 2.5e-01)
#' prange <- data.frame(min = pars - pars * 0.75,
#' max = pars + pars * 0.75)
#' t <- floor(seq(0, 365, length.out = 10))
#' out <- memc_format_sensout(config = MEND_model, t = t, pars = pars,
#' parRange = prange, dist = "latin", num = 10)
#' to_plot <- memc_format_sensout(out)
#' ggplot(data = to_plot) +
#'    geom_line(aes(time, Mean)) +
#'    geom_ribbon(aes(time, ymin = Min, ymax = Max), alpha = 0.5) +
#'    facet_wrap("variable", scales = "free")
#'}
 memc_format_sensout <- function(obj){

  cond <- any(class(obj)[[1]] %in% c("sensRange", "sensFun"))
  assert_that(cond)

  if(class(obj)[[1]] == "sensRange"){
    out <- summary(obj)
    names(out)[1] <- "time"
    vars <- gsub(pattern = "\\.|[[:digit:]]+", replacement = "", x = row.names(out))
    out$variable <- vars
    row.names(out) <- NULL
  }

  if(class(obj)[[1]] == "sensFun"){

    out <- data.table::as.data.table(obj)
    names(out)[1] <- "time"
    names(out)[2] <- "variable"
    params <- names(out)[3:ncol(out)]

    out <- data.table::melt(out, id.vars = c("time", "variable"),
                            value.name = "value", variable.name = "parameter")

  }

  return(out)
}
