
#' Load the parameter values into a environment
#'
#' @param ptable data.table containing the following columns: parameter, value, and units.
#' @param state A numeric vector of the different carbon pool states that will be used to up date the preset values read in from the env, default set to NULL will use the entries in the env object.
#' @importFrom assertthat assert_that
#' @family helper functions
#' @noRd
internal_load_params <- function(ptable, state){

  # Make sure that the parameter table has the correct names.
  req_names <- c("parameter", "description", "units", "value")
  assert_that(has_name(ptable, req_names))

  # Make sure that the state is a vector
  assert_that(is.numeric(state))
  assert_that(is.character(names(state)), msg = "state must be named")

  # Create the empty environment
  env <- new.env(parent = emptyenv())

  # Extract the parameter tables.
  p        <- ptable$value
  names(p) <- ptable$parameter

  mapply(function(pname, pvalue){
    assign(pname, pvalue, envir = env)
  }, pname = names(p), pvalue = p)

  mapply(function(sname, svalue){
    assign(sname, svalue, envir = env)
  }, sname = names(state), svalue = state)

  return(env)

}


#' Modify the environment to use new values
#'
#' @param env environment created by \code{internal_load_params}
#' @param new a vector of new parameter or state variables
#' @importFrom assertthat assert_that
#' @family helper functions
#' @noRd
modify_env <- function(env, new){

  # Check the the inputs
  assert_that(is.character(names(new)))
  assert_that(is.numeric(new))
  assert_that(is.environment(env))

  mapply(function(nname, nvalue){
    assign(nname, nvalue, envir = env)
  }, nname = names(new), nvalue = new)

  return(env)

}


#' Set up a model configuration
#'
#' @param params data.table containing the following columns: parameter, value, and units.
#' @param config knitr_kable describing the model configuration
#' @param state a vector of the initial state values, must be named
#' @param carbon_pools_func a function defining the carbon pools, by default is set to the \code{carbon_pools}
#' @param carbon_fluxes_func a function defining the carbon fluxes between pools, by default is set to \code{carbon_fluxes}
#' @param name default set to MEND, is the string name of the model.
#' @param F1 string for the F1 carbon flux microbial kinetics, by default set to MM ("Michaelis Menten")
#' @importFrom assertthat assert_that
#' @export
#' @family helper functions
configure_model <- function(params, state, carbon_pools_func = carbon_pools, carbon_fluxes_func, name = "MEND", F1 = "MM"){

  # Load all the parameter tables and the state variables into an environment.
  env <- internal_load_params(ptable = params, state = state)

  # Make sure that the pools and fluxes are being read in as functions and that
  # the have not been used in place of one another.
  assert_that(is.function(carbon_pools_func))
  flux_func <- carbon_fluxes_func(env = env, F1 = F1)
  assert_that(is.list(flux_func))
  assert_that(length(flux_func) == 2)
  assert_that(is.function(flux_func[["flux_function"]]))
  req_args <-c('env', 'state', 'params')
  assert_that(has_args(flux_func[["flux_function"]], req_args), msg = "problem with carbon_pools_func format")

  # Make sure that the carbon pools function.
  assert_that(is.function(carbon_pools_func))
  req_args <- c('t', 'env', 'flux_function')
  assert_that(has_args(carbon_pools_func, req_args), msg = "problem with carbon_pools_func format")



  # Check to make sure that there are no missing parameter values, an error will be
  # thrown here if the params table is missing an entry.
  rslt <- carbon_pools_func(t = 1, env, flux_function = flux_func)
  assert_that(all(is.numeric(unlist(rslt))))

  # Format the table
  table <- make_config_table(name, flux_func$flux_table)
  custom_config_table_message(table)

  model_object <- list("name" = name,
                       "config" = table,
                       "params" = params,
                       "state" = state,
                       "env" = env,
                       "carbon_pools_func" = carbon_pools_func,
                       "carbon_fluxes_func" = flux_func)

  return(model_object)

}


#' Solve a MEMC configuration
#'
#' @param mod model object created by \code{make_model}
#' @param time a vector of the time setps
#' @param params default set to NULL, will then use the parameter table read in with the "mod" object.
#' @param state default set to NULL, will then use the state read read in with the "mod" object.
#' @param ... additional arguments that can be read into \code{deSolve::ode}
#' @return a long formatted data.table of the simulation results
#' @importFrom assertthat assert_that has_args
#' @importFrom deSolve ode
#' @export
#' @family helper functions
solve_model <- function(mod, time, params = NULL, state = NULL, ...){

  assert_that(is.list(mod))
  req_names <-  c("name", "params", "state" ,"env", "carbon_pools_func", "carbon_fluxes_func")
  assert_that(has_name(x = mod, which = req_names), msg = "mod must a model object created by configure_model")
  assert_that((is.null(params) | is.data.frame(params)))
  assert_that((is.null(state) | is.vector(state)))
  assert_that(all(is.numeric(time)))

  env <- mod[["env"]]


  # Update model parameter & initial state values if needed.
  if(!is.null(params)){
    p <- params$value
    names(p) <- params$parameter
    modify_env(env, p)
  } else if (is.null(params)){
    params <- mod[["params"]]
  }
  if(!is.null(state)){
    modify_env(env, state)
  } else if (is.null(state)){
    state <- mod[['state']]
  }

  # Solve the model
  c_pool <- mod$carbon_pools_func
  c_flux <- mod$carbon_fluxes_func

  rslt  <- ode(y = state,
               times = time,
               func = c_pool,
               flux_function = c_flux,
               env = env,
               parms = params,
               ...)


  # Now format the results into a nice data frame instead of a wide  matrix.
  out <- data.table::melt(data.table::as.data.table(rslt),
                          measure.vars = names(mod$state),
                          variable.name = "variable",
                          value.name = 'value')
  out$units <- 'mg C/g soil'

  if (is.null(mod$name)){
    name <- "(unnamed)"
  } else {
    name <- mod$name
  }
  out$name <- name

  return(out)

}


#' Update the parameter table with new values
#'
#' @param new_params a named vector of parameter values to update the param table.
#' @param param_table data.table containing the following columns: parameter, value, and units, this is the basic parameter table that will be updated with the new values.
#' @return updated data.table containing the new parameter values
#' @importFrom assertthat assert_that
#' @export
#' @family helper functions
#' @family parameters
update_params <- function(new_params, param_table){

  req_paramtable_names <- c("parameter", "description", "units", "value")
  assert_that(has_name(x = param_table, which = req_paramtable_names),
              msg = paste0("param_table is missing a required column: ", paste0(req_paramtable_names, collapse = ", ")))


  assert_that(!is.null(names(new_params)), msg = "new params must be named")
  pnames <- names(new_params)
  assert_that(all(pnames %in% param_table$parameter), msg = "new_params must refer to a parameter already existing in param_table")
  assert_that(is.numeric(new_params))

  # Update the param_table with the new values! To avoid there being a dependency on
  # the order in which the new_params are read into the function use a for loop to itterate
  # over all the parameters to be updated.
  for (p in pnames){
    index <- which(param_table$parameter == p)
    param_table$value[index] <- new_params[[p]]
  }

  return(param_table)

}


#' Modify a flux functions
#'
#' @param flux_func a function that will return a list of functions that modify how carbon moves between the carbon pools.
#' @param F1 default set to MM, could be one of "MM", "RMM", "ECA", "LM", each referring to a type of kinetics
#' @return a list of carbon flux functions
#' @importFrom assertthat assert_that has_args
#' @export
#' @family helper functions
modify_fluxes_func <- function(flux_func, F1 = "MM"){

  # Check the inputs
  assert_that(is.function(flux_func))
  assert_that(has_args(flux_func, "env"))
  assert_that(has_args(flux_func, "state"))
  assert_that(has_args(flux_func, "params"))

  assert_that(is.character(F1))
  assert_that(length(F1) == 1)
  assert_that(any(F1 %in% c("MM", "RMM", "ECA", "LM")))

  function(env, state=NULL, params=NULL){

    # Update model parameter & initial state values if needed.
    if(!is.null(params)){
      p <- params$value
      names(p) <- params$parameter
      modify_env(env, p)

    }
    if(!is.null(state)){
      modify_env(env, state)
    }
    with(as.list(env), {

      # Set up the original functions.
      fluxes  <- flux_func(env = env)

      if(F1 == "MM"){
        fluxes[["F1"]] = function(){
          # DOC uptake by microbial biomass.
          # Michaelis menten kinetics
          (1/E.c) * (V.d + m.r) * B * D /(K.d + D)
        }
      } else if(F1 == "RMM") {
        fluxes[["F1"]] = function(){
          # DOC uptake by microbial biomass.
          # Reverse michaelis menten kinetics
          (1/E.c) * (V.d + m.r) * B * D /(K.d + B)
        }
      } else if(F1 == "ECA"){
        fluxes[["F1"]] = function(){
          # DOC uptake by microbial biomass, note that this uses ECA kinetics.
          (1/E.c) * (V.d + m.r) * B * D /(K.d + B + D)
        }
      } else if(F1 == "LM") {
        message("needs to be implemented")
      }

      return(fluxes)

    })
  }

}


#' Using information provided to the \code{configure_model} make a table model of flux information.
#'
#' @param name string for the model configuration.
#' @param Fs data.frame describing the types of fluxes.
#' @return knitr_kable
#' @importFrom assertthat assert_that
#' @noRd
make_config_table <- function(name, Fs){

  assert_that(is.data.frame(Fs))
  table <- cbind("Model" = name, Fs)
  out <- knitr::kable(table)

  return(out)

}

#' Message the configuration table
#'
#' @param x knitr_kable \code{make_config_table}
#' @return message to the terminal
#' @importFrom assertthat assert_that
#' @noRd
custom_config_table_message <- function(x){

  # Make sure that the table was created by make_config_table
  assert_that(class(x) ==  "knitr_kable")

  # Message!
  message(paste0(out, collapse = '\n'))

}





