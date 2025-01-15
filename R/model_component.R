#' Define the carbon pool flux functions based on the model dynamics
#'
#' @param parms MEMC parameter table
#' @param DOMuptake string indicator for type of dynamics used for the DOM decomposition
#' @param POMdecomp string indicator for type of dynamics used for the POM decomposition
#' @param MBdecay string indicator for type of dynamics used to model MB decay
#' @seealso dynamics
#' @return A list of functions to be used for calculating each flux
#' (the names of the list are the flux names: F1, F2, etc).
#' @noRd
#' @family internal
c_flux_functions_internal <-
    function(p,
             DOMuptake = "MM",
             POMdecomp = "MM",
             MBdecay = "LM") {

        flux_functions <- list()
        if (DOMuptake == "MM") {
            flux_functions[["F1"]] = function(MB, DOM) {
                p[["V_d"]] * MB * DOM / (p[["K_d"]] + DOM)
            }
        } else if (DOMuptake == "RMM") {
            flux_functions[["F1"]] = function(MB, DOM) {
                p[["V_d"]] * MB * DOM / (p[["K_d"]] + MB)
            }
        } else if (DOMuptake == "ECA") {
            flux_functions[["F1"]] = function(MB, DOM) {
                p[["V_d"]] * MB * DOM / (p[["K_d"]] + DOM + MB)
            }
        } else if (DOMuptake == "LM") {
            flux_functions[["F1"]] = function(MB, DOM) {
                p[["V_d"]] * DOM
            }
        } else {
            stop("Unknown DOMuptake!")
        }

        if (POMdecomp == "MM") {
            flux_functions[["F2"]] = function(EP, POM) {
                (p[["V_p"]] * EP * POM) / (p[["K_p"]] + POM)
            }
        } else if (POMdecomp == "RMM") {
            flux_functions[["F2"]] = function(EP, POM) {
                (p[["V_p"]] * EP * POM) / (p[["K_p"]] + EP)
            }
        } else if (POMdecomp == "ECA") {
            flux_functions[["F2"]] = function(EP, POM) {
                (p[["V_p"]] * EP * POM) / (p[["K_p"]] + POM + EP)
            }
        } else if (POMdecomp == "LM") {
            flux_functions[["F2"]] = function(EP, POM) {
                p[["V_p"]] * POM
            }
        } else {
            stop("Unknown POMdecomp!")
        }

        flux_functions[["F3"]] = function(EM, MOM) {
            (p[["V_m"]] * EM * MOM) / (p[["K_m"]] + MOM)
        }
        flux_functions[["F4"]] = function(DOM, QOM) {
            p[["K_ads"]] * DOM * (1 - QOM /p[["Q_max"]])
        }
        flux_functions[["F5"]] = function(QOM) {
            p[["K_des"]] * QOM / p[["Q_max"]]
        }

        if (MBdecay == "DD") {
            stopifnot(p[["dd_beta"]] > 1)
            flux_functions[["F6"]] = function(MB) {
                (1 - p[["p_ep"]] - p[["p_em"]]) * 0.4 * p[["V_d"]] * (MB ^ p[["dd_beta"]])
            }
        } else if (MBdecay == "LM") {
            stopifnot(p[["dd_beta"]] == 1)
            flux_functions[["F6"]] = function(MB) {
                (1 - p[["p_ep"]] - p[["p_em"]]) * 0.4 * p[["V_d"]] * (MB ^ p[["dd_beta"]])
            }
        } else {
            stop("Unknown MBdecay!")
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
#' @param DOMuptake string indicator for type of dynamics used to model DOM decomposition
#' @param POMdecomp string indicator for type of dynamics used to model POM decomposition
#' @param MBdecay string indicator for type of dynamics used to model MB decay
#' @return The derivatives of each pool, i.e. instantaneous change, as a list.
#' @noRd
#' @family internal
carbon_pool_derivs <-
    function(t,
             state,
             p,
             DOMuptake,
             POMdecomp,
             MBdecay) {

        # Get the carbon flux functions (`cff`) to use
        cff <- c_flux_functions_internal(p = p,
                                         DOMuptake = DOMuptake,
                                         POMdecomp = POMdecomp,
                                         MBdecay = MBdecay)

        with(as.list(state), {
            F1 <-    cff$F1(MB = MB, DOM = DOM) # DOM loss
            F2 <-    cff$F2(EP = EP, POM = POM) # POM loss
            F3 <-    cff$F3(EM = EM, MOM = MOM) # MOM loss
            F4 <-    cff$F4(DOM = DOM, QOM = QOM)
            F5 <-    cff$F5(QOM = QOM)
            F6 <-    cff$F6(MB = MB) # MB decay to POM/DOM
            F7_ep <- cff$F7_ep(MB = MB) # MB flux to EP
            F7_em <- cff$F7_em(MB = MB) # MB flux to EM
            F8_ep <- cff$F8_ep(EP = EP) # EP loss
            F8_em <- cff$F8_em(EM = EM) # EM loss

            # Define the system of differential equations that describe
            # the changes in the carbon pool states_
            # -----------------------------------------------------------
            # POM = particulate organic carbon
            dPOM <- (1 - p[["g_d"]]) * F6 - F2 + p[["Input_POM"]]
            # MOM = mineral-associated organic carbon (MOC)
            dMOM <- (1 - p[["f_d"]]) * F2 - F3
            # QOMO = active layer of MOC
            dQOM <- F4 - F5
            # MB = microbial biomass carbon
            dMB <- F1 * p[["CUE"]] - F6 - (F7_ep + F7_em)
            # DOM = dissolved organic carbon
            dDOM <-
                p[["f_d"]] * F2 + p[["g_d"]] * F6 + F3 + (F8_em + F8_ep) -
                F1 - (F4 - F5) + p[["Input_DOM"]]
            # EP = carbon stored as extra-cellular enzymes
            dEP <- F7_em - F8_ep
            # EM = carbon stored as extra-cellular enzymes
            dEM <- F7_em - F8_em
            # IC = inorganic carbon (CO2)
            dIC <- F1 * (1 - p[["CUE"]])
            # Tot = the total carbon pool
            dTot <- -F1 * (1 - p[["CUE"]]) + (p[["Input_POM"]] + p[["Input_DOM"]])

            # Return derivatives (instantaneous changes in the pools)
            return(list(c(dPOM, dMOM, dQOM, dMB, dDOM, dEP, dEM, dIC, dTot)))

        })

    }


#' Internal solve model function
#'
#' @param mod model object created from \code{\link{memc_configure}}
#' @param time numeric vector of the timestamps of when to solve the model
#' @return The result from calling \code{\link[deSolve]{ode}}
#' @seealso [memc_all_models()]
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
        DOMuptake = mod[["table"]][["DOMuptake"]],
        POMdecomp = mod[["table"]][["POMdecomp"]],
        MBdecay = mod[["table"]][["MBdecay"]], ...)

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
#' by \code{\link{memc_all_models}}
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
#' memc_solve(MEND_model, time = 0:10)
memc_solve <-
    function(mod,
             time,
             params = NULL,
             state = NULL,
             ...) {

        # Update the model configuration with new parameter and initial state values
        mod <- update_config(mod = mod,
                             new = c(params, state))

        # Check the arguments
        assert_that(all(time >= 0))
        assert_that(is_memc_config(obj = mod))
        assert_that(is_param_table(table = mod$params))
        assert_that(is_state_vector(state = mod$state))

        results <- sm_internal(mod = mod, time = time, ...)
        out <- sm_format_out(rslt = results, mod = mod)

        return(out)

    }
