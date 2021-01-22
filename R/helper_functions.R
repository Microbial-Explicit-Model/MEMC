#' Assign the paramter values
#'
#' Define parameters based on the a parameter data table that is read in.
#'
#' @param dt data.table containing the following columns: parameter, value, and units.
#' @param req default set to NULL, if a string vector is read will check to make sure that
#' all of the required parameters are defined in the data table.
#' @return Nothing, but has defined the elements of the parameter column as objects in
#' the environment.
#' @importFrom assertthat assert_that has_name
assign_parameters <- function(dt, req = NULL){

  assert_that(data.table::is.data.table(dt))
  has_name(x = dt, which = c('parameter', 'value', 'units'))
  assert_that(is.character(dt[['parameter']]))
  assert_that(is.numeric(dt[['value']]))
  if(!is.null(req)){
    missing <- !req %in% dt[['parameter']]
    assert_that(all(!missing), msg = paste('dt missing parameters: ', paste0(req[missing], collapse = ', ')))
  }

  mapply(assign, x = dt$parameter, value = dt$value, inherits = TRUE)

  # Return nothing
  return(NULL)
}



#' MEMC solver function
#'
#' Based on user defined carbon pools and flux functions solve a system of equations.
#'
#' @param params data.table containing the following columns: parameter, value, and units.
#' @param time a vector of the time setps
#' @param state a vector of the intial state values, must be named
#' @param carbon_pools_func a function defining the carbon  pools
#' @param carbon_fluxes_func a function defining the carbon fluxes between pools
#' @param ... additional arguments that can be read into \code{deSolve::ode}
#' @return a long formatted data.table of the simulation results
#' @importFrom assertthat assert_that
#' @export
#' @family helper function
solver <- function(params, time, state, carbon_pools_func, carbon_fluxes_func, ...){

  # Make sure that the pools and fluxes are being read in as functions and that
  # the have not been used in place of one another.
  assert_that(is.function(carbon_pools_func))
  req_args  <-c('t', 'state', 'parms', 'flux_function')
  pool_args <- as.vector(names(formals(carbon_pools_func)))
  missing   <- req_args[!req_args %in% pool_args]
  assert_that(length(missing) ==  0,  msg = paste('carbon_pool_func missing required arguments: ', paste(missing, collapse = ', ')))


  assert_that(is.function(carbon_fluxes_func))
  req_args  <- c("state", "parms")
  flux_args <- as.vector(names(formals(carbon_fluxes_func)))
  missing   <- req_args[!req_args %in% flux_args]
  assert_that(length(missing) == 0, msg = paste('carbon_flux_func missing required arguments: ', paste(missing, collapse = ', ')))
  assert_that(all(!grepl(pattern = 'flux', flux_args)), msg = 'check carbon_fluxes_func, make sure the pool function is not being used in stead.')


  rslt  <- deSolve::ode(y = state,      # initial state estimates
                        times = time,  # times to solve over
                        parms = params,  # parameter table
                        func = carbon_pools_func, # the pool represenation we are intrested in
                        flux_function = carbon_fluxes_func,  # the flux represenation we are intrested in
                        ...) # extra ode arguments

  # Now format the results into a nice data frame instead of a wide  matrix.
  out <- data.table::melt(data.table::as.data.table(rslt),  measure.vars = names(state),
                          variable.name = "variable", value.name = 'value')
  out$units <- 'mg C/g soil'
  # TODO might want to remove the out units thing
  return(out)

}

