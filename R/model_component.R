#' Define the carbon pool flux functions based on the model dynamics
#'
#' @param parms MEMC parameter table
#' @param F1 string indicator for type of dynamics used for the DOC decomposition
#' @param F2 string indicator for type of dynamics used for the POC decomposition
#' @param F8 string indicator for type of dynamics used to model MB mortality
#' @seealso dynamics
#' @return A list of functions to be used for calculating each flux
#' (the names of the list are the flux names: F1, F2, etc).
#' @noRd
#' @family internal
c_flux_functions_internal <-
  function(p,
           F1 = "MM",
           F2 = "MM",
           F8 = "LM") {
    flux_functions <- list()
    if (F1 == "MM") {
      flux_functions[["F1"]] = function(MB, DOC) {
        p[["V_d"]] * MB * DOC / (p[["K_d"]] + DOC)
      }
    } else if (F1 == "RMM") {
      flux_functions[["F1"]] = function(MB, DOC) {
        p[["V_d"]] * MB * DOC / (p[["K_d"]] + MB)
      }
    } else if (F1 == "ECA") {
      flux_functions[["F1"]] = function(MB, DOC) {
        p[["V_d"]] * MB * DOC / (p[["K_d"]] + DOC + MB)
      }
    } else if (F1 == "LM") {
      flux_functions[["F1"]] = function(MB, DOC) {
        p[["V_d"]] * DOC
      }
    } else {
      stop("Unknown F1!")
    }
    
    if (F2 == "MM") {
      flux_functions[["F2"]] = function(EP, POC) {
        (p[["V_p"]] * EP * POC) / (p[["K_p"]] + POC)
      }
    } else if (F2 == "RMM") {
      flux_functions[["F2"]] = function(EP, POC) {
        (p[["V_p"]] * EP * POC) / (p[["K_p"]] + EP)
      }
    } else if (F2 == "ECA") {
      flux_functions[["F2"]] = function(EP, POC) {
        (p[["V_p"]] * EP * POC) / (p[["K_p"]] + POC + EP)
      }
    } else if (F2 == "LM") {
      flux_functions[["F2"]] = function(EP, POC) {
        p[["V_p"]] * POC
      }
    } else {
      stop("Unknown F2!")
    }
    
    flux_functions[["F3"]] = function(EM, MOC) {
      (p[["V_m"]] * EM * MOC) / (p[["K_m"]] + MOC)
    }
    flux_functions[["F4"]] = function(DOC, QOC) {
      p[["K_ads"]] * DOC * (1 - QOC / p[["Q_max"]])
    }
    flux_functions[["F5"]] = function(QOC) {
      p[["K_des"]] * QOC / p[["Q_max"]]
    }
    
    if (F8 == "DD") {
      stopifnot(p[["dd_beta"]] > 1)
      flux_functions[["F6"]] = function(MB) {
        (1 - p[["p_ep"]] - p[["p_em"]]) * 0.4 * p[["V_d"]] * (MB ^ p[["dd_beta"]])
      }
    } else if (F8 == "LM") {
      stopifnot(p[["dd_beta"]] == 1)
      flux_functions[["F6"]] = function(MB) {
        (1 - p[["p_ep"]] - p[["p_em"]]) * 0.4 * p[["V_d"]] * (MB ^ p[["dd_beta"]])
      }
    } else {
      stop("Unknown F8!")
    }
    
    flux_functions[["F7_ep"]] = function(MB) {
      p[["p_ep"]] * MB *  0.4 * p[["V_d"]]
    }
    flux_functions[["F7_em"]] = function(MB) {
      p[["p_em"]] * MB *  0.4 * p[["V_d"]]
    }
    flux_functions[["F8_ep"]] = function(EP) {
      p[["r_ep"]] * EP
    }
    flux_functions[["F8_em"]] = function(EM) {
      p[["r_em"]] * EM
    }
    
    return(flux_functions)
    
  }


#' Calculate the derivatives for each carbon pool
#'
#' @param t numeric when to solve the model
#' @param state MEMC vector of the pool values
#' @param parms MEMC parameter table
#' @param F1 string indicator for type of dynamics used to model DOC decomposition
#' @param F2 string indicator for type of dynamics used to model POC decomposition
#' @param F8 string indicator for type of dynamics used to model MB mortality
#' @return The derivatives of each pool, i.e. instantaneous change, as a list.
#' @noRd
#' @family internal
carbon_pool_derivs <-
  function(t,
           state,
           p,
           F1,
           F2,
           F8) {
    # Get the carbon flux functions (`cff`) to use
    cff <- c_flux_functions_internal(
      p = p,
      F1 = F1,
      F2 = F2,
      F8 = F8
    )
    
    with(as.list(state), {
      F1 <-    cff$F1(MB = MB, DOC = DOC) # DOC loss
      F2 <-    cff$F2(EP = EP, POC = POC) # POC loss
      F3 <-    cff$F3(EM = EM, MOC = MOC) # MOC loss
      F4 <-    cff$F4(DOC = DOC, QOC = QOC)
      F5 <-    cff$F5(QOC = QOC)
      F6 <-    cff$F6(MB = MB) # MB decay to POC/DOC
      F7_ep <- cff$F7_ep(MB = MB) # MB flux to EP
      F7_em <- cff$F7_em(MB = MB) # MB flux to EM
      F8_ep <- cff$F8_ep(EP = EP) # EP loss
      F8_em <- cff$F8_em(EM = EM) # EM loss
      
      # Define the system of differential equations that describe
      # the changes in the carbon pool states_
      # -----------------------------------------------------------
      # POC = particulate organic carbon
      dPOC <- (1 - p[["g_d"]]) * F6 - F2 + p[["Input_POC"]]
      # MOC = mineral-associated organic carbon (MOC)
      dMOC <- (1 - p[["f_d"]]) * F2 - F3
      # QOCO = active layer of MOC
      dQOC <- F4 - F5
      # MB = microbial biomass carbon
      dMB <- F1 * p[["CUE"]] - F6 - (F7_ep + F7_em)
      # DOC = dissolved organic carbon
      dDOC <-
        p[["f_d"]] * F2 + p[["g_d"]] * F6 + F3 + (F8_em + F8_ep) -
        F1 - (F4 - F5) + p[["Input_DOC"]]
      # EP = carbon stored as extra-cellular enzymes
      dEP <- F7_em - F8_ep
      # EM = carbon stored as extra-cellular enzymes
      dEM <- F7_em - F8_em
      # IC = inorganic carbon (CO2)
      dIC <- F1 * (1 - p[["CUE"]])
      # Tot = the total carbon pool
      dTot <-
        -F1 * (1 - p[["CUE"]]) + (p[["Input_POC"]] + p[["Input_DOC"]])
      
      # Return derivatives (instantaneous changes in the pools)
      return(list(c(
        dPOC, dMOC, dQOC, dMB, dDOC, dEP, dEM, dIC, dTot
      )))
      
    })
    
  }


#' Internal solve model function
#'
#' @param mod model object created from \code{\link{memc_configure}}
#' @param time numeric vector of the timestamps of when to solve the model
#' @return The result from calling \code{\link[deSolve]{ode}}
#' @seealso [memc_all_configs()]
#' @noRd
#' @family internal
sm_internal <- function(mod, time, ...) {
  p <- mod[["params"]][["value"]]
  names(p) <- mod[["params"]][["parameter"]]
  
  # Check that all the parameters that are fractions are less than 1
  frac_params <- c("f_d", "g_d", "p_ep", "p_em")
  frac_params_vals <- p[names(p) %in% frac_params]
  assert_that(all(0 < frac_params_vals & frac_params_vals < 1),
              msg = "parameters f_d, g_d, p_ep, and p_em must be between 0 and 1")
  
  rslt <- deSolve::ode(
    y = mod[["state"]],
    times = time,
    func = carbon_pool_derivs,
    parms = p,
    F1 = mod[["table"]][["F1"]],
    F2 = mod[["table"]][["F2"]],
    F8 = mod[["table"]][["F8"]],
    ...
  )
  
  return(rslt)
  
}


#' Format the output into something that is nice to return to solve model
#'
#' @param rslt object returned from \code{\link{sm_internal}}
#' @param mod object created from \code{\link{memc_configure}}
#' @return A nicely-formatted data frame with the results.
#' @noRd
#' @family internal
sm_format_out <- function(rslt, mod) {
  # Format the results into a nice data frame instead of a wide matrix.
  out <- data.table::melt(
    data.table::as.data.table(rslt),
    measure.vars = names(mod$state),
    variable.name = "variable",
    value.name = 'value'
  )
  out$units <- 'mg C/g soil'
  
  if (is.null(mod$name)) {
    name <- "(unnamed)"
  } else {
    name <- mod$name
  }
  out$name <- name
  
  return(out)
  
}

#' Solve a MEMC configuration
#'
#' @param mod model object, for example one of the list entries returned
#' by \code{\link{memc_all_configs}}
#' @param time daily time steps for model run
#' @param params default set to NULL, will then use the parameter table
#' read in with the \code{mod} object
#' @param state default set to NULL, will then use the state read read
#' in with the \code{mod} object
#' @param ... additional arguments passed to \code{\link[deSolve]{ode}}
#' @return A long-formatted \code{\link[data.table]{data.table}} of the
#' simulation results, with columns:
#' \item{time}{Time point, days}
#' \item{variable}{Name of model carbon pool}
#' \item{value}{Value of the pool}
#' \item{units}{Units of the pool value}
#' \item{name}{Model name used}
#' @seealso \code{\link{memc_configure}}
#' @importFrom assertthat assert_that has_args
#' @importFrom deSolve ode
#' @export
#' @family helper
#' @examples
#' out <- memc_solve(MEND_config, time = 0:10)
#' plot(out)
memc_solve <-
  function(mod,
           time,
           params = NULL,
           state = NULL,
           ...) {
    # Update the model configuration with new parameter and initial state values
    mod <- memc_update_config(mod = mod,
                              new = c(params, state))
    
    # Check the arguments
    assert_that(all(time >= 0))
    assert_that(is_memc_config(obj = mod))
    assert_that(is_param_table(table = mod$params))
    assert_that(is_state_vector(state = mod$state))
    
    results <- sm_internal(mod = mod, time = time, ...)
    out <- sm_format_out(rslt = results, mod = mod)
    class(out) <- c("memc_solve", class(out))
    
    return(out)
    
  }
