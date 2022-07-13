#' Checks that it is a MEMC model configuration
#'
#' @param obj list object to check to see if it is a model configuration
#' @return TRUE or FALSE indicator
#' @importFrom assertthat has_name
#' @family helper functions
#' @noRd
# TODO should add check to see if the types of objects are correct
is_memc_config <- function(obj){

  cond <- is.list(obj)
  cond <- c(cond, has_name(x = obj, which = c("name", "table", "params", "state", "derivs")))
  return(all(cond))
}


#' Update the parameter table with new values
#'
#' @param new_params a named vector of parameter values to update the param table.
#' @param param_table data.table containing the following columns: parameter, value, and units, this is the basic parameter table that will be updated with the new values.
#' @return updated data.table containing the new parameter values
#' @import assertthat
#' @export
#' @family helper functions
#' @family parameters
update_params <- function(new_params, param_table){

  req_paramtable_names <- c("parameter", "description", "units", "value")
  assert_that(has_name(x = param_table, which = req_paramtable_names),
              msg = paste0("param_table is missing a required column: ", paste0(req_paramtable_names, collapse = ", ")))


  assert_that(!is.null(names(new_params)), msg = "new params must be named")
  pnames <- names(new_params)
  dne <- pnames[!pnames %in% param_table$parameter]
  assert_that(length(dne) < 1, msg = paste0( "new_params must refer to a parameter already existing in param_table \n the following params are not recognized: ",
                                             paste0(dne, collapse = ', ')))

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


#' TODO update
#'
#' @param new_vals update
#' @param state update
#' @import assertthat
#' @family helper functions
update_state <- function(new_vals, state){

  req_names <- c("P", "M", "Q", "B", "D", "EP", "EM", "IC", "Tot")
  assert_that(has_name(x = state, which = req_names),
              msg = paste0("state is missing a required column: ", paste0(req_names, collapse = ", ")))
  assert_that(is.vector(state))

  assert_that(is.vector(new_vals))
  assert_that(all(names(new_vals) %in% req_names))


  for (n in names(new_vals)){
    index <- which(names(state) == n)
    state[[n]] <- new_vals[[n]]
  }

  return(state)

}


update_config <- function(mod, params = NULL, state = NULL){

  assert_that(is_memc_config(mod))

  if(!is.null(params)){
    mod[["params"]] <-  update_params(new_params = params, param_table = mod[["params"]])
  }

  if(!is.null(state)){

    mod[["state"]] <-  update_state(new_vals = state, state = mod[["state"]])

  }

  return(mod)

}



