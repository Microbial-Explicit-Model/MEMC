#' Define the carbon pools
#'
#' \code{carbon_pools} Defines the system of equations that
#' describe the state of the carbon pools, it follows a general 5-pool scheme that is commonly
#' adopted in microbial-explicit models following \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#'
#' @param t is for time
#' @param env environment created by \code{internal_load_params}
#' @param state A numeric vector of the different carbon pool states to update the env, by default set to NULL
#' @param params A data frame of the parameters to update the env, by default set to NULL
#' @param flux_function a function that will return a list of functions that modify how carbon moves between
#' the carbon pools.
#' @return A list of the state variables
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @importFrom assertthat assert_that has_name
#' @family carbon pool functions
#' @export
carbon_pools <-
  function(t,
           env,
           state = NULL,
           params = NULL,
           flux_function = carbon_fluxes(POMdecomp = "MM")) {
    assert_that(is.numeric(t))
    fx <- flux_function
    assert_that(is.list(flux_function))
    assert_that(length(fx) == 2)
    assert_that(is.function(fx[["flux_function"]]))

    # Update model parameter & initial state values if needed.
    if (!is.null(params)) {
      p <- params$value
      names(p) <- params$parameter
      modify_env(env, p)

    }
    if (!is.null(state)) {
      modify_env(env, state)
    }

    with(as.list(env), {
      # Define the fluxes and check to make sure they meet the requirements to be used
      # by the MEND carbon pool structure.
      #ff <- flux_function(env)[["flux_function"]]
      #fluxes <- ff(env)
      ff <- fx[["flux_function"]]
      fluxes <- ff(env)

      expected_fluxes <- rep('F', length.out = length(1:8))
      expected_fluxes <-
        c(paste0(expected_fluxes, 1:8),
          'F9.ep',
          'F9.em',
          'F10.ep',
          'F10.em')
      assertthat::assert_that(assertthat::has_name(x = fluxes, which = expected_fluxes))
      assertthat::assert_that(all(unlist(lapply(
        fluxes, is.function
      ))), msg = 'fluxes input must be a list of functions')

      # Define the system of differential equations to describes the
      # changes in the carbon pool states.
      # -----------------------------------------------------------
      # P = particulate organic carbon
      dP <- I.p + (1 - g.d) * fluxes$F8() - fluxes$F2()
      # M = mineral-associated organic carbon (MOC)
      dM <- (1 - f.d) * fluxes$F2() - fluxes$F3()
      # Q = active layer of MOC
      dQ <- fluxes$F6() - fluxes$F7()
      # B = microbial biomass carbon
      dB <-
        fluxes$F1() - (fluxes$F4() + fluxes$F5()) - fluxes$F8() - (fluxes$F9.ep() + fluxes$F9.em())
      # D = dissolved organic carbon
      dD <-
        I.d + f.d * fluxes$F2() + g.d * fluxes$F8() + fluxes$F3() + (fluxes$F10.em() + fluxes$F10.ep()) - fluxes$F1() - (fluxes$F6() - fluxes$F7())
      # EP = carbon stored as extra-cellular enzymes
      dEP <- fluxes$F9.em() - fluxes$F10.ep()
      # EM = carbon stored as extra-cellular enzymes
      dEM <- fluxes$F9.em() - fluxes$F10.em()
      # IC = inorganic carbon (CO2)
      dIC <- fluxes$F4() + fluxes$F5()
      # Tot = the total carbon pool
      dTot <- I.p + I.d - (fluxes$F4() + fluxes$F5())

      # Return outputs
      list(c(dP, dM, dQ, dB, dD, dEP, dEM, dIC, dTot))
    })
  }

#' A copy of carbon fluxes for internal package use.
#'
#' \code{carbon_fluxes_internal} Defines a system of equations that
#' describe fluxes between the general 5-pool scheme based on \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' By returning a list of named functions. The default configuration is set up to follow the fluxes defined in \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' More advanced users may choose to change the flux. It is the default connfiguration.
#'
#' @param env an environment of the initial state and parameter values created by \code{internal_load_params}.
#' @param state A numeric vector of the different carbon pool states that will be used to up date the preset values read in from the env, default set to NULL will use the entries in the env object.
#' @param params A data frame of the parameters that will be used to up date the entries in the env environment, by default set to NULL.
#' @return A list of functions that calculate the fluxes between carbon pools.
#' @family carbon flux functions
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @importFrom assertthat assert_that has_name
#' @noRd
carbon_fluxes_internal <-
  function(env, state = NULL, params = NULL) {
    # Update model parameter & initial state values if needed.
    if (!is.null(params)) {
      p <- params$value
      names(p) <- params$parameter
      modify_env(env, p)

    }
    if (!is.null(state)) {
      modify_env(env, state)
    }

    with(as.list(env), {
      fxn_list <- list(
        "F1" = function() {
          # DOC uptake by microbial biomass.
          (1 / E.c) * (V.d + m.r) * B * D / (K.d + D)
        },
        "F2" = function() {
          # POC decomposition
          V.p * EP * P / (K.p + P)
        },
        "F3" = function() {
          # Break down of mineralized organic carbon
          V.m * EM * M / (K.m + M)
        },
        "F4" = function() {
          # Microbial respiration from biomass growth
          (1 / E.c - 1) * V.d * B * D / (K.d + D)
        },
        "F5" = function() {
          # Metabolic/maintenance microbial respiration
          (1 / E.c - 1) * m.r * B * D / (K.d + D)
        },
        "F6" = function() {
          # Adsorption of DOC to mineral-associated organic carbon
          K.ads * D * (1 - Q / Q.max)
        },
        "F7" = function() {
          # Desorption of mineral-associated organic carbon to DOC
          K.des * Q / Q.max
        },
        "F8" = function() {
          # Carbon loss due to microbial biomass mortality
          (1 - p.ep - p.em) * m.r * B
        },
        "F9.ep" = function() {
          # Enzyme production
          p.ep * m.r * B
        },
        "F9.em" = function() {
          # Enzyme production
          p.em * m.r * B
        },
        "F10.ep" = function() {
          # Enzyme turn over
          r.ep * EP
        },
        "F10.em" = function() {
          # Enzyme turn over
          r.em * EM
        }
      )

      return(fxn_list)

    })

  }

#' Define the carbon fluxes.
#'
#' \code{carbon_fluxes} Defines a system of equations that
#' describe fluxes between the general 5-pool scheme based on \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' By returning a list of named functions. The default configuration is set up to follow the fluxes defined in \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' More advanced users may choose to change the flux.

#' @param DOMdecomp string indicating the type of enzyme kinetics used in the microbial decomposition of DOM, one  of the following "MM", "RMM", "ECA", or "LM", see \code{\link{kinetics}} for more details.
#' @param POMdecomp string indicating the type of enzyme kinetics used in the microbial decomposition of POM, one  of the following "MM", "RMM", "ECA", or "LM", see \code{\link{kinetics}} for more details.
#' @param env an environment of the initial state and parameter values created by \code{internal_load_params}, by default it is set to an empty environment.
#' @param state A numeric vector of the different carbon pool states that will be used to up date the preset values read in from the env, default set to NULL will use the entries in the env object.
#' @param params A data frame of the parameters that will be used to up date the entries in the env environment, by default set to NULL.
#' @return A list of functions that calculate the fluxes between carbon pools.
#' @family carbon flux functions
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @importFrom assertthat assert_that has_name
#' @export
carbon_fluxes <-
  function(DOMdecomp = "MM",
           POMdecomp = "MM",
           env = NULL,
           state = NULL,
           params = NULL) {
    # Check the inputs
    assert_that(any(is.null(env) | is.environment(env)))
    assert_that(any(is.null(state) | is.list(state)))
    assert_that(any(is.null(state) | is.data.frame(params)))

    assert_that(all(sapply(list(POMdecomp, DOMdecomp), is.character)))
    assert_that(sum(DOMdecomp %in% c("MM", "RMM", "ECA", "LM")) == 1)
    assert_that(sum(POMdecomp %in% c("MM", "RMM", "ECA", "LM")) == 1)


    # Create the list to store the output.
    out <- list()

    # Store information about the fluxes being used.
    out[["flux_table"]] <- data.table::data.table("DOMdecomp" = DOMdecomp,
                             "POMdecomp" = POMdecomp)

    # Store the fluxes.
    out[["flux_function"]] <- function(env,
                                       state = NULL,
                                       params = NULL) {
      # Update model parameter & initial state values if needed.
      if (!is.null(params)) {
        p <- params$value
        names(p) <- params$parameter
        modify_env(env, p)

      }
      if (!is.null(state)) {
        modify_env(env, state)
      }
      with(as.list(env), {
        # Set up the original functions.
        fluxes  <- carbon_fluxes_internal(env = env)

        # If else statement determining the kinetics used in the decompostion of the DOM.
        if (DOMdecomp == "MM") {
          fluxes[["F1"]] = function() {
            # DOC uptake by microbial biomass.
            # Michaelis menten kinetics
            (1 / E.c) * (V.d + m.r) * B * D / (K.d + D)
          }
        } else if (DOMdecomp == "RMM") {
          fluxes[["F1"]] = function() {
            # DOC uptake by microbial biomass.
            # Reverse michaelis menten kinetics
            (1 / E.c) * (V.d + m.r) * B * D / (K.d + B)
          }
        } else if (DOMdecomp == "ECA") {
          fluxes[["F1"]] = function() {
            # DOC uptake by microbial biomass, note that this uses ECA kinetics.
            (1 / E.c) * (V.d + m.r) * B * D / (K.d + B + D)
          }
        } else if (DOMdecomp == "LM") {
          fluxes[["F1"]] = function() {
            # DOC uptake by microbial biomass, note that this uses a linear model of kinetics.
            V.d * B * D / (K.d + B)
          }
        }

        # If else statement determining the kinetics used in the decompostion of the POM.
        if (POMdecomp == "MM") {
          fluxes[["F2"]] = function() {
            # POC decomposition
            # michaelis menten kinetics
            # DOC uptake by microbial biomass.
            V.p * EP * P / (K.p + P)
          }
        } else if (POMdecomp == "RMM") {
          fluxes[["F2"]] = function() {
            # POC decomposition
            # Reverse michaelis menten kinetics
            V.p * EP * P / (K.p + EP)
          }
        } else if (POMdecomp == "ECA") {
          fluxes[["F2"]] = function() {
            # POC uptake by microbial biomass, note that this uses ECA kinetics.
            V.p * EP * P / (K.p + EP + B)
          }
        } else if (POMdecomp == "LM") {
          fluxes[["F2"]] = function() {
            # POC uptake by microbial biomass, note that this uses a linear model of kinetics.
            V.p * EP * P / (K.p + B)
          }
        }

        return(fluxes)

      })
    }

    return(out)

  }
