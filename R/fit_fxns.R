
#' Calculate the model cost between output and obs data frame
#'
#' Internally used within \link{make_objective_function}, this is a MEMC wrapper for \link[pkg:FME]{modCost}.
#'
#' @param output data.frame returned by
#' @param obs data.frame of observations or the data to fit the model to
#' @return modCost object
#' @family fit functions
memc_modcost <- function(output, obs){

  # Subset the output data so that the variables & time steps are the same.
  output <- as.data.frame(output)
  subset_output <- output[output$time %in% obs$time, names(obs)]

  # Make sure the dims are correct.
  #assertthat::assert_that(all(dim(subset_output) == dim(obs)))

  out <- FME::modCost(model = subset_output, obs = obs, x = "time")

  return(out)

}


#' Make the objective function
#'
#' Internally used within \link{memc_modfit}
#'
#' @param mod memc model object created by \link{configure_model}
#' @param obs data.frame of the observations must contain values for MEMC state variables
#' @param x numeric vector with the values to be optimized, may be a model parameter or an initial state value
#' @return function to be used as the objective function by mod cost
#' @importFrom assertthat has_name assert_that
#' @family fit functions
make_objective_function <- function(mod, obs, x){

  # Some checks that we will want to once, before the internal MSE function to minimize
  # redundancy of checks.
  xnames <- names(x)
  assertthat::assert_that(is.data.frame(obs))
  assertthat::assert_that(all(names(obs) %in% c("time", "P", "M", "Q", "B", "D", "EP", "EM", "IC", "Tot")))
  assertthat::assert_that(is_memc_config(mod))
  assertthat::assert_that(all(xnames %in% c(mod$params$parameter, names(mod$state))))

  fxn <- function(x){

    # split x into its parameter & initial state values and update the model
    # inputs.
    state_index <- which(xnames %in% names(mod$state))
    if (length(state_index) == 0 ) {
      new_state <- NULL
    } else {
      new_val <- x[state_index]
      names(new_val) <- xnames[state_index]

      new_state <- mod$state
      new_state[names(new_val)] <- new_val

    }
    param_index <- which(xnames %in% mod$params$parameter)
    if (length(param_index) == 0) {
      updated_p_table <-  NULL
    } else {
      new_params <- x[param_index]
      names(new_params) <- xnames[param_index]
      updated_p_table <- update_params(new_params, mod$params)
    }

    output <- sm_internal(mod = mod, time = unique(obs$time), params = updated_p_table, state = new_state)
    cost <- memc_modcost(output, obs)
    return(cost)
  }

  return(fxn)

}



#' Constrain MEMC model to observational data
#'
#' @param mod memc model object created by \link{configure_model}
#' @param obs data.frame of the observations must contain values for MEMC state variables
#' @param x numeric vector with the values to be optimized, may be a model parameter or an initial state value, must be named
#' @param ... additional arguments passed to \link[pkg:FME]{modCost}
#' @return function to be used as the objective function by mod cost
#' @importFrom assertthat has_name assert_that
#' @family fit functions
#' @export
memc_modfit <- function(mod, obs, x, ...){

  objective <- make_objective_function(mod, obs, x)
  out <- FME::modFit(p = x, f = objective, ...)

  return(out)

}



