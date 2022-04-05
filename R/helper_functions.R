
#' Checks that it is a MEMC model configuration
#'
#' @param obj list object to check to see if it is a model configuration
#' @return TRUE or FALSE indicator
#' @importFrom assertthat has_name
#' @family helper functions
#' @noRd
is_memc_config <- function(obj){

  cond <- is.list(obj)
  cond <- c(cond, has_name(x = obj, which = c("name", "config", "params", "state", "env",
                                          "carbon_pools_func", "carbon_fluxes_func")))
  return(all(cond))
}



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
#' @param state a vector of the initial state values, must be named
#' @param carbon_pools_func a function defining the carbon pools, by default is set to the \code{carbon_pools}
#' @param carbon_fluxes_func a function defining the carbon fluxes between pools, by default is set to \code{carbon_fluxes}
#' @param name string name of the model configuration, default set to "MEND".
#' @param DOMdecomp string indicating the type of enzyme kinetics used in the microbial decomposition of DOM, one  of the following "MM", "RMM", "ECA", or "LM", see \code{\link{kinetics}} for more details.
#' @param POMdecomp string indicating the type of enzyme kinetics used in the microbial decomposition of POM, one  of the following "MM", "RMM", "ECA", or "LM", see \code{\link{kinetics}} for more details.
#' @param MBdecay string indicating microbial decay, one  of the following "MM", "RMM", "ECA", or "LM", see \code{\link{kinetics}} for more details.
#' @importFrom assertthat assert_that
#' @export
#' @family helper functions
configure_model <- function(params,
                            state,
                            carbon_pools_func = carbon_pools,
                            carbon_fluxes_func = carbon_fluxes,
                            name = "MEND",
                            DOMdecomp = "MM",
                            POMdecomp = "MM",
                            MBdecay = "LM"){

  # Load all the parameter tables and the state variables into an environment.
  env <- internal_load_params(ptable = params, state = state)

  assert_that(is.function(carbon_fluxes_func))
  flux_func <- carbon_fluxes_func(env = env, POMdecomp = POMdecomp, DOMdecomp = DOMdecomp, MBdecay = MBdecay)
  assert_that(flux_func$flux_table$POMdecom == POMdecomp)
  assert_that(is.list(flux_func))
  assert_that(length(flux_func) == 2)
  assert_that(is.function(flux_func[["flux_function"]]))
  req_args <-c('env', 'state', 'params')
  assert_that(has_args(flux_func[["flux_function"]], req_args), msg = "problem with carbon_pools_func format")
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
  # the order in which the new_params are read into the function use a for loop to iterate
  # over all the parameters to be updated.
  for (p in pnames){
    index <- which(param_table$parameter == p)
    param_table$value[index] <- new_params[[p]]
  }

  return(param_table)

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


  return(table)

}

#' Message the configuration table
#'
#' @param x knitr_kable \code{make_config_table}
#' @return message to the terminal
#' @importFrom assertthat assert_that
#' @noRd
custom_config_table_message <- function(x){

  # Make sure that the table was created by make_config_table
  assert_that(is.data.frame(x))
  out <- knitr::kable(x)

  # Message!
  message(paste0(out, collapse = '\n'))

}

