#' Update the parameter table with new values
#'
#' @param new_params a named vector of parameter values to update the param table.
#' @param param_table data.table containing the following columns: parameter, value, and units, this is the basic parameter table that will be updated with the new values.
#' @return Updated \code{\link[data.table]{data.table}} containing the new parameter values.
#' @importFrom assertthat assert_that
#' @export
#' @family helper functions
#' @family parameters
memc_update_params <- function(new_params, param_table) {
  assert_that(is_param_table(param_table))
  assert_that(!is.null(names(new_params)), msg = "new params must be named")
  pnames <- names(new_params)
  dne <- pnames[!pnames %in% param_table$parameter]
  assert_that(
    length(dne) < 1,
    msg = paste0(
      "new_params must refer to a parameter already existing in param_table \n the following params are not recognized: ",
      paste0(dne, collapse = ', ')
    )
  )
  
  assert_that(all(pnames %in% param_table$parameter), msg = "new_params must refer to a parameter already existing in param_table")
  assert_that(is.numeric(new_params))
  
  # Update the param_table with the new values! To avoid a dependency on
  # the order in which the new_params are read into the function, use a for
  # loop to iterate over all the parameters to be updated.
  for (p in pnames) {
    index <- which(param_table$parameter == p)
    param_table$value[index] <- new_params[[p]]
  }
  
  return(param_table)
  
}


#' Update the state vector values; this is for internal function use
#'
#' @param new_vals update
#' @param state update
#' @importFrom assertthat assert_that
#' @family helper functions
update_state <- function(new_vals, state) {
  assert_that(is_state_vector(state))
  
  req_names <-
    c("POM", "MOM", "QOM", "MB", "DOM", "EP", "EM", "IC", "Tot")
  assert_that(is.vector(new_vals))
  assert_that(all(names(new_vals) %in% req_names))
  
  for (n in names(new_vals)) {
    index <- which(names(state) == n)
    state[[n]] <- new_vals[[n]]
  }
  
  return(state)
  
}


#' Update a model configuration
#'
#' @param mod a MEMC model configuration object created by \code{memc_configure}
#' @param new vector containing the parameters and or initial pool values
#' @importFrom assertthat assert_that
#' @family helper functions
#' @export
memc_update_config <- function(mod, new = NULL) {
  assert_that(is_memc_config(mod))
  
  if (is.null(new)) {
    return(mod)
  }
  
  x <- split_param_state(new)
  
  if (length(x$params) >= 1) {
    mod[["params"]] <-
      memc_update_params(new_params = x$params, param_table = mod[["params"]])
  }
  
  if (length(x$state) >= 1) {
    mod[["state"]] <-
      update_state(new_vals = x$state, state = mod[["state"]])
    
  }
  
  return(mod)
  
}


#' Set up a model configuration
#'
#' @param params data.table containing the following columns: parameter, value, and units.
#' @param state a vector of the initial state values, must be named
#' @param name string name of the model configuration, default set to "MEND".
#' @param DOMuptake string indicating the dynamics used to model microbial decomposition of DOM, one  of the following "MM", "RMM", or "ECA"
#' @param POMdecomp string indicating the dynamics used to model microbial decomposition of POM, one  of the following "MM", "RMM", "ECA", or "LM"
#' @param MBdecay string indicating microbial decay, one  of the following ""LM" or "DD"
#' @return memc_single_config object of the name, dynamics, parameters and starting state values
#' @importFrom assertthat assert_that
#' @export
#' @family helper functions
#' @examples
#' # Modify the MEND model
#' m <- MEND_config
#' m_mod <- memc_configure(m$params, m$state, "MEND_modified", POMdecomp = "LM")
#' summary(m_mod)
#' memc_solve(m_mod, 0:10)
memc_configure <- function(params,
                           state,
                           name = "unnamed",
                           DOMuptake = "MM",
                           POMdecomp = "MM",
                           MBdecay = "LM") {
  # Check the arguments
  assert_that(is_param_table(params))
  assert_that(all(sapply(
    list(POMdecomp, DOMuptake, MBdecay), is.character
  )))
  assert_that(sum(DOMuptake %in% c("MM", "RMM", "ECA", "LM")) == 1,
              msg = 'DOMuptake must be "MM", "RMM", "ECA"')
  assert_that(sum(POMdecomp %in% c("MM", "RMM", "ECA", "LM")) == 1,
              msg = 'POMdecomp must be "MM", "RMM", "ECA", "LM"')
  assert_that(sum(MBdecay %in% c("LM", "DD")) == 1,
              msg = 'MBdecay must be "LM" or "DD"')
  
  # Format the table
  table <- data.frame(
    "model" = name,
    "DOMuptake" = DOMuptake,
    "POMdecomp" = POMdecomp,
    "MBdecay" = MBdecay
  )
  
  model_object <- list(
    "name" = name,
    "table" = table,
    "params" = params,
    "state" = state
  )
  class(model_object) <-
    c("memc_single_config", class(model_object))
  
  return(model_object)
  
}


#' Split up a vector into parameters and state values
#'
#' @param x input vector containing state/parameter values to use in a model run
#' @return A list containing the state and param vectors.
#' @importFrom assertthat assert_that
#' @noRd
#' @family internal
split_param_state <- function(x) {
  assert_that(is.character(names(x)))
  assert_that(is.numeric(x))
  assert_that(all(names(x) %in% c(
    names(MEMC::memc_initial_state),
    MEMC::memc_params$parameter
  )),
  msg = "value not recognized as a parameter or state")
  
  params_index <-
    which(names(x) %in%  MEMC::memc_params$parameter)
  state_index <-
    which(names(x) %in% names(MEMC::memc_initial_state))
  
  if (length(params_index) == 0) {
    params <- NULL
  } else {
    params <- x[params_index]
  }
  
  if (length(state_index) == 0) {
    state <- NULL
  } else {
    state <- x[state_index]
  }
  
  out <- list(params = params, state = state)
  
  return(out)
  
}

#' Return a color palette for the the default MEMC model configurations and other simulations
#'
#' @param name input vector containing the model names to return the color codes for, default will return colors for all the model configurations,
#' @return A vector containing color hex codes for the different model configurations.
#' @importFrom assertthat assert_that
#' @export
#' @examples
#' \dontrun{
#' out <- memc_solve(MEND_config, time = 0:100)
#' ggplot(data = out) +
#'   geom_line(aes(time, value, color = name)) +
#'   scale_color_manual(values = memc_colorPalette()) + 
#'   facet_wrap("variable", scales = "free")
#' }
memc_colorPalette <- function(name = NULL) {
  # The color Palette for the different model configurations.
  color_vec <- c(
    "MIMCS" = "#FEC22C",
    "MEND" = "#FE9527",
    "CORPSE" = "#7F7F7F",
    "BAMS" = "#FE8BD8",
    "COMISSION" = "#D783FD",
    "MEMS" = "#1494FC"
  )
  
  assert_that(length(MEMC::memc_all_configs) == length(color_vec),
              msg = "Problem with default palette size")
  
  # If no names are specified return the full color vector
  if (is.null(name)) {
    return(color_vec)
    
  } else {
    # If there is no new configurations to consider then
    # return the specific color palette otherwise use
    # the rainbow palette to assign colors to the new
    # configurations
    default_configs <- intersect(name, names(color_vec))
    new_configs <- setdiff(name, names(color_vec))
    
    if (identical(new_configs, character(0))) {
      out <- color_vec[default_configs]
      return(out)
    } else {
      new_colors <- grDevices::rainbow(length(new_configs))
      out <- c(color_vec[default_configs], new_colors)
      names(out) <- c(default_configs, new_configs)
      return(out)
    }
    
    
  }
  # If the name is not included as one of the default
  # MEMC supported configurations then use the
  # base rainbow pot
  setdiff(name, names(MEMC::memc_all_configs))
  
  
  
  if (is.null(name)) {
    return(color_vec)
  } else {
    
  }
  
}
