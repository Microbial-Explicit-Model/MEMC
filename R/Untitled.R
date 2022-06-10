#'
#'
#'
#'
#'
#' Data<-read.csv("Ultisol_control.csv",header=TRUE)
#'
#' Objective <- function(x, parset = names(x)) {
#'   pars[parset] <- x
#'   tout <- seq(0, 365, by = 1) ## output times
#'   out <- solveCOM(pars, tout)
#'   return(modCost(model = out, obs = Data))## Model cost
#' }
#'
#'
#'
#' setup_objective_function <- function(mod, x, obs){
#'
#'   # First thing to do is check the inputs!
#'   assert_that(is_memc_config(mod))
#'
#'   # The observation data set
#'   assert_that("time" %in% names(obs))
#'   var_names <- setdiff(names(obs), "time")
#'   unkown_vars <- var_names[!var_names %in% names(mod$state)]
#'   assert_that(length(unkown_vars) == 0,  msg = paste(paste0(unkown_vars, collapse = ", "), "are unkown variables in obs"))
#'
#'   # The params/inital values being optimized
#'   assert_that(is.character(names(x)), msg = "x must be named")
#'   unkown_x <- names(x)[!names(x) %in% c(mod$params$parameter, names(mod$state))]
#'   assert_that(length(unkown_x) == 0,  msg = paste(paste0(unkown_x, collapse = ", "), "are must be a MEMC model parameter or inital state value"))
#'
#'   objective_function <- function(x){
#'
#'     # Partion the x into parameters and state variables.
#'     pi <- which(names(x) %in% mod$params$parameter)
#'     new_params <- x[pi]
#'     new_p_table <- update_params(new_params, mod$params)
#'
#'     si <- which(names(x) %in% names(mod$state))
#'     new_state <- x[si]
#'
#'     if length(new_p_table) == 0 {
#'       new_p_table <- NULL
#'     }
#'
#'     if length(new_state) == 0 {
#'       new_state <- NULL
#'     }
#'
#'     model_rslts <- sm_internal(mod = mod, time = obs$time, params = new_p_table, state = new_state)
#'     cost <- FME::modCost(model = model_rslts, obs = obs)
#'     return(cost)
#'
#'   }
#'
#'   return(objective_function)
#'
#' }
#'
#' p <- 26.41739
#' names(p) <- 'V.d'
#' objective_function <- setup_objective_function(mod, p, obs)
#'
#'
#'
#' #' Calculate the MSE between output and obs data frame
#' #'
#' #' @param output data.frame returned by
#' #' @param obs data.frame of observations or the data to fit the model to
#' #' @return numeric value of the mean squared error
#' #' @family internal
#' #' @noRd
#' internal_MSE_func <- function(output, obs){
#'
#'   # Subset the output data so that the variables & time steps are the same.
#'   subset_output <- output[output$time %in% obs$time, names(obs)]
#'
#'   assert_that(all(dim(subset_output) == dim(obs)))
#'
#'   # Calculate the multi variable mean squared error
#'   SE <- (subset_output - obs)^2
#'   MSE <- apply(X = SE, MARGIN = 2, FUN = mean, na.rm = TRUE)
#'   MSE_vals <- MSE[!names(MSE) == "time"]
#'   multi_MSE <- mean(MSE_vals)
#'   return(MSE_vals)
#'
#' }
#'
#'
#'
#' make_objective_function <- function(mod, obs, p){
#'
#'   # Some checks that we will want to once, before the internal MSE function is being called to make this faster and faseter
#'   output <- as.data.frame(output)
#'   assert_that(is.data.frame(output))
#'   assert_that(all(names(output) == c("time", "P", "M", "Q", "B", "D", "EP", "EM", "IC", "Tot")))
#'   assert_that(is.data.frame(obs))
#'   assert_that(all(names(obs) %in% names(output)))
#'   assert_that(all(obs$time %in% output$time)) ## not really sure how to fit to different data sets....
#'
#'
#'   function(p){
#'     updated_p_table <- update_params(new_params = p, param_table = mod$params) # not sure if the time would be
#'     output <- sm_internal(mod = mod, time = obs$time, params = updated_p_table)
#'     MSE <- internal_MSE_func(output, obs)
#'   }
#'
#'
#' }
#'
