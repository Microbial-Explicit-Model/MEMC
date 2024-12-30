#' Checks that an object is a MEMC model configuration
#'
#' @param obj list object to check to see if it is a model configuration
#' @return TRUE or FALSE indicator.
#' @importFrom assertthat has_name
#' @family helper functions
#' @noRd
is_memc_config <- function(obj) {
  cond <- is.list(obj)

  # Expected format of a MEMC model object
  types <- c("name" = "character", "table" = "data.frame",
             "params" = "data.frame", "state" = "numeric")
  # Check identical lengths
  cond <- c(cond, length(types) == length(obj))
  # Check individual entries of the `obj` list
  for(i in names(types)) {
      cond <- c(cond, class(obj[[i]]) == types[i])
  }

  return(all(cond))

}


#' Checks that a data frame is a properly formatted MEMC table
#'
#' Will return a TRUE value if passes tests or throw an error if
#' the conditions are not met
#'
#' @param table data frame of MEMC model parameter values
#' @return TRUE or FALSE indicator.
#' @importFrom assertthat assert_that has_name
#' @family helper functions
#' @noRd
is_param_table <- function(table) {
  # Check the column names of parameter table.
  req_names <- c("parameter", "description", "units", "value")
  assert_that(
    has_name(x = table, which = req_names),
    msg = paste0(
      "param_table is missing a required column: ",
      paste0(req_names, collapse = ", ")
    )
  )

  # Make sure that the parameter table contains values for all the required parameters.
  req_entries <- MEMC::default_params$parameter
  missing <- req_entries[!req_entries %in% table$parameter]
  assert_that(
    length(missing) == 0,
    msg = paste0(
      "param_table is missing a parameter value(s) for: ",
      paste0(missing, collapse = ", ")
    )
  )

  # Make sure there aren't any sneaky unknown parameters being read in.
  extra_params <-
    table$parameter[!table$parameter %in% MEMC::default_params$parameter]
  assert_that(
    length(extra_params) == 0,
    msg = paste0(
      "param_table contains unkown parameter value(s): ",
      paste0(extra_params, collapse = ", ")
    )
  )

  return(TRUE)

}


#' Checks to see if a vector meets the state requirements
#'
#' @param state named vector of the MEMC states
#' @return TRUE or FALSE indicator.
#' @importFrom assertthat assert_that
#' @family helper functions
#' @noRd
is_state_vector <- function(state) {
  assert_that(is.character(names(state)))
  assert_that(is.vector(state))
  assert_that(is.numeric(state))

  req_names <- c("POM", "MOM", "QOM", "MB", "DOM", "EP", "EM", "IC", "Tot")
  missing <- req_names[!req_names %in% names(state)]
  assert_that(length(missing) == 0,
              msg = paste0(
                "state is missing a value(s) for: ",
                paste0(missing, collapse = ", ")
              ))

  out_of_order <- names(state) == req_names
  assert_that(all(out_of_order),
              msg = paste0(
                "entires must be in the correct order: ",
                paste0(req_names, collapse = ", ")
              ))

  return(TRUE)

}
