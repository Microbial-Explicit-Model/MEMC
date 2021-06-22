#' Solve different single P pool MEND configurations
#'
#'
#' @param parameters data.table containing the following columns: parameter, value, and units. Default values are stored as pacakge see \code{MEND_params}.
#' @param time a vector of the time setps.
#' @param inital_state a numeric vector of the different MEND carbon pool states. Default values are probvide as package data see \code{MEND_initalState}.
#' @return a data frame of output from the different MEND configurations, \code{MEND}, \code{MEND_RM}, \code{MEND_ECA}, and \code{MEND_MILLEN}.
#' @family intercomparison
#' @family MEND
#' @export
compare_MEND <- function(parameters, time, inital_state){

  results <- data.table::as.data.table(rbind(cbind(model = 'MEND', MEND(parameters = parameters, time = time, inital_state = inital_state)),
                                             cbind(model = 'MEND_RM', MEND_RM(parameters = parameters, time = time, inital_state = inital_state)),
                                             cbind(model = 'MEND_ECA', MEND_ECA(parameters = parameters, time = time, inital_state = inital_state)),
                                             cbind(model = 'MEND_MILLEN', MEND_MILLEN(parameters = parameters, time = time, inital_state = inital_state))))
  return(results)

}

#' Solve different two P pool MEND configurations
#'
#'
#' @param parameters data.table containing the following columns: parameter, value, and units. Default values are stored as pacakge see \code{MEND2_params}.
#' @param time a vector of the time setps.
#' @param inital_state a numeric vector of the different MEND carbon pool states. Default values are probvide as package data see \code{MEND2_initalState}.
#' @return a data frame of output from the different MEND configurations, \code{MEND2}, \code{MEND2_RM}, \code{MEND2_ECA}, and \code{MEND2_MILLEN}.
#' @family intercomparison
#' @family MEND2
#' @export
compare_MEND2 <- function(parameters, time, inital_state){


  results <- data.table::as.data.table(rbind(cbind(model = 'MEND2', MEND2(parameters = parameters, time = time, inital_state = inital_state)),
                                             cbind(model = 'MEND2_RM', MEND2_RM(parameters = parameters, time = time, inital_state = inital_state)),
                                             cbind(model = 'MEND2_ECA', MEND2_ECA(parameters = parameters, time = time, inital_state = inital_state)),
                                             cbind(model = 'MEND2_MILLEN', MEND2_MILLEN(parameters = parameters, time = time, inital_state = inital_state))))
  return(results)

}


