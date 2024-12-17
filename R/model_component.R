#' Define the carbon pool fluxes
#'
#' @param parms MEMC parameter table
#' @param DOMuptake string indicator for type of dynamics used for the DOM decomposition
#' @param POMdecomp string indicator for type of dynamics used for the POM decomposition
#' @param MBdecay string indicator for type of dynamics used to model MB decay
#' @noRd
#' @family internal
carbon_fluxes_internal <-
    function(p,
             DOMuptake = "MM",
             POMdecomp = "MM",
             MBdecay = "LM") {

        fluxes <- list()
        if (DOMuptake == "MM") {
            fluxes[["F1"]] = function(MB, DOM) {
                p[["V_d"]] * MB * DOM / (p[["K_d"]] + DOM)
            }
        } else if (DOMuptake == "RMM") {
            fluxes[["F1"]] = function(MB, DOM) {
                p[["V_d"]] * MB * DOM / (p[["K_d"]] + MB)
            }
        } else if (DOMuptake == "ECA") {
            fluxes[["F1"]] = function(MB, DOM) {
                p[["V_d"]] * MB * DOM / (p[["K_d"]] + DOM + MB)
            }
        } else if (DOMuptake == "LM"){
            fluxes[["F1"]] = function(MB, DOM) {
                p[["V_d"]] * DOM
            }
        }

        if (POMdecomp == "MM") {
            fluxes[["F2"]] = function(EP, POM) {
                (p[["V_p"]] * EP * POM) / (p[["K_p"]] + POM)
            }
        } else if (POMdecomp == "RMM") {
            fluxes[["F2"]] = function(EP, POM) {
                (p[["V_p"]] * EP * POM) / (p[["K_p"]] + EP)
            }
        } else if (POMdecomp == "ECA") {
            fluxes[["F2"]] = function(EP, POM) {
                (p[["V_p"]] * EP * POM) / (p[["K_p"]] + POM + EP)
            }
        } else if (POMdecomp == "LM") {
            fluxes[["F2"]] = function(EP, POM) {
                p[["V_p"]] * POM
            }
        }

        fluxes[["F3"]] = function(EM, MOM) {
            (p[["V_m"]] * EM * MOM) / (p[["K_m"]] + MOM)
        }
        fluxes[["F4"]] = function(DOM, QOM) {
            p[["K_ads"]] * DOM * (1 - QOM /p[["Q_max"]])
        }
        fluxes[["F5"]] = function(QOM) {
            p[["K_des"]] * QOM / p[["Q_max"]]
        }

        if (MBdecay == "DD") {
            stopifnot(exprs = {p[["dd_beta"]] > 1})
            fluxes[["F6"]] = function(MB) {
                (1 - p[["p_ep"]] - p[["p_em"]]) * 0.4 * p[["V_d"]] * (MB ^ p[["dd_beta"]])
            }
        } else if (MBdecay == "LM") {
            stopifnot(exprs = {p[["dd_beta"]] == 1})
            fluxes[["F6"]] = function(MB) {
                (1 - p[["p_ep"]] - p[["p_em"]]) * 0.4 * p[["V_d"]] * (MB ^ p[["dd_beta"]])
            }
        }

        fluxes[["F7_ep"]] = function(MB) {
            p[["p_ep"]] * MB *  0.4 * p[["V_d"]]
        }
        fluxes[["F7_em"]] = function(MB) {
            p[["p_em"]] * MB *  0.4 * p[["V_d"]]
        }
        fluxes[["F8_ep"]] = function(EP) {
            p[["r_ep"]] * EP
        }
        fluxes[["F8_em"]] = function(EM) {
            p[["r_em"]] * EM
        }

        return(fluxes)

    }


#' Define the carbon pool model
#'
#' @param t numeric when to solve the model
#' @param state MEMC vector of the pool values
#' @param parms MEMC parameter table
#' @param DOMuptake string indicator for type of dynamics used for the DOM decomposition
#' @param POMdecomp string indicator for type of dynamics used for the POM decomposition
#' @param MBdecay string indicator for type of dynamics used to model MB decay
#' @noRd
#' @family internal
carbon_pool_derivs <-
    function(t,
             state,
             p,
             DOMuptake,
             POMdecomp,
             MBdecay) {

        fluxes <- carbon_fluxes_internal(p = p,
                                         DOMuptake = DOMuptake,
                                         POMdecomp = POMdecomp,
                                         MBdecay = MBdecay)

        with(as.list(state), {
            F1 <- fluxes$F1(MB = MB, DOM = DOM)
            F2 <- fluxes$F2(EP = EP, POM = POM)
            F3 <- fluxes$F3(EM = EM, MOM = MOM)
            F4 <- fluxes$F4(DOM = DOM, QOM = QOM)
            F5 <- fluxes$F5(QOM = QOM)
            F6 <- fluxes$F6(MB = MB)
            F7_ep <- fluxes$F7_ep(MB = MB)
            F7_em <- fluxes$F7_em(MB = MB)
            F8_ep <- fluxes$F8_ep(EP = EP)
            F8_em <- fluxes$F8_em(EM = EM)

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
                p[["f_d"]] * F2 + p[["g_d"]] * F6 + F3 + (F8_em + F8_ep) - F1 - (F4 - F5) + p[["Input_DOM"]]
            # EP = carbon stored as extra-cellular enzymes
            dEP <- F7_em - F8_ep
            # EM = carbon stored as extra-cellular enzymes
            dEM <- F7_em - F8_em
            # IC = inorganic carbon (CO2)
            dIC <- F1 * (1 - p[["CUE"]])
            # Tot = the total carbon pool
            dTot <- -F1 * (1 - p[["CUE"]]) +  (p[["Input_POM"]] + p[["Input_DOM"]])

            # Return outputs
            return(list(c(dPOM, dMOM, dQOM, dMB, dDOM, dEP, dEM, dIC, dTot)))

        })

    }


#' Internal solve model function
#'
#' @param mod object created from \code{\link{configure_model}}
#' @param time numeric vector of the time stamps of when to solve the model
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
#' @param mod object created from \code{\link{configure_model}}
#' @noRd
#' @family internal
sm_format_out <- function(rslt, mod) {
    # Now format the results into a nice data frame instead of a wide  matrix.
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
#' @param mod model object
#' @param time a vector of the time steps
#' @param params default set to NULL, will then use the parameter table
#' read in with the \code{mod} object
#' @param state default set to NULL, will then use the state read read
#' in with the \code{mod} object
#' @param ... additional arguments passed to \code{\link[deSolve]{ode}}
#' @return A long-formatted \code{\link[data.table]{data.table}} of the
#' simulation results; \code{time} = hour.
#' @importFrom assertthat assert_that has_args
#' @importFrom deSolve ode
#' @export
#' @family helper
solve_model <-
    function(mod,
             time,
             params = NULL,
             state = NULL,
             ...) {

        # Update the model configuration with new parameter and initial state values
        mod <- update_config(mod = mod,
                             new = c(params, state))

        # Check the arguments
        assert_that(is_memc_config(obj = mod))
        assert_that(is_param_table(table = mod$params))
        assert_that(is_state_vector(state = mod$state))

        results <- sm_internal(mod = mod, time = time, ...)
        out <- sm_format_out(rslt = results, mod = mod)

        return(out)

    }
