#' Summary table of memc_all_configs
#'
#' Provides a summary of dynamics used in `memc_all_configs`.
#'
#' @param object An object of class `memc_all_configs`.
#' @param ... Additional arguments (ignored).
#' @export
#' @method summary memc_all_configs
#' @noRd
summary.memc_all_configs <- function(object, ...) {
  if (!inherits(object, "memc_all_configs"))
    stop("Object is not of class 'memc_all_configs'")
  
  tables <-
    sapply(object, function(x)
      x["table"],
      simplify = TRUE, USE.NAMES = FALSE)
  
  single_df <- do.call(what = "rbind", args = tables)
  rownames(single_df) <- NULL
  
  out <- knitr::kable(single_df)
  return(out)
  
}


#' Summary table for a single memc model
#'
#' Provides a summary of dynamics used in `memc_single_config`.
#'
#' @param object An object of class `memc_single_config`.
#' @param ... Additional arguments (ignored).
#' @export
#' @method summary memc_single_config
#' @noRd
summary.memc_single_config <- function(object, ...) {
  if (!inherits(object, "memc_single_config"))
    stop("Object is not of class 'memc_single_config'")
  
  out <- knitr::kable(object$table)
  return(out)
  
}


#' Return the memc_all_configs object
#'
#' Provides details of all the configurations included in `memc_all_configs`.
#'
#' @param x An object of class `memc_all_configs`.
#' @param ... Additional arguments (ignored).
#' @export
#' @method print memc_all_configs
#' @noRd
print.memc_all_configs <- function(x, ...) {
  if (!inherits(x, "memc_all_configs"))
    stop("Object is not of class 'memc_all_configs'")
  
  # Remove attributes by unclassing to simplify the user experience
  object_no_attributes <- unclass(x)
  print.default(object_no_attributes)
  invisible(object_no_attributes)
  
}


#' Return a full memc model object
#'
#' Provides details of all single model configuration
#'
#' @param x An object of class `memc_single_config`.
#' @param ... Additional arguments (ignored).
#' @export
#' @method print memc_single_config
#' @noRd
print.memc_single_config <- function(x, ...) {
  if (!inherits(x, "memc_single_config"))
    stop("Object is not of class 'memc_single_config'")
  
  # Remove attributes by unclassing to simplify the user experience
  object_no_attributes <- unclass(x)
  print.default(object_no_attributes)
  invisible(object_no_attributes)
  
}



#' Quick plot the results returned from memc_solve
#'
#'
#' @param x An object of class `memc_solve`.
#' @param y Ignore
#' @param ... Additional arguments (ignored).
#' @export
#' @import ggplot2
#' @method plot memc_solve
plot.memc_solve <- function(x, y, ...) {
  if (!inherits(x, "memc_solve"))
    stop("Object is not of class 'memc_solve'")
  
  # Silence package checks
  time <- value <- name <- NULL
  
  # Save a copy of the units and the colors to use when plotting.
  units <- unique(x$units)
  color_palette <- memc_colorPalette(unique(x$name))
  
  # Make a line plot for the different soil carbon pools
  ggplot(data = x) +
    geom_line(aes(time, value, color = name)) +
    facet_wrap("variable", scales = "free") +
    labs(x = "Time (days)", y = units) +
    scale_color_manual(values = color_palette) +
    theme_bw() +
    theme(legend.title = element_blank()) ->
    plot
  
  return(plot)
}


#' Quick plot the results returned from memc_sensRange
#'
#'
#' @param x An object of class `memc_sensRange`.
#' @param y Ignore
#' @param ... additional arguments center, lower, upper that may be used to define the line and ribbon boundaries.
#' @export
#' @import ggplot2
#' @method plot memc_sensRange
plot.memc_sensRange <- function(x, y, ...) {
  if (!inherits(x, "memc_sensRange"))
    stop("Object is not of class 'memc_sensRange'")
  
  # Silence package checks
  time <- name <- NULL
  
  
  # The default information to use in the ribbon plot
  center <- "Mean"
  lower <- "Min"
  upper <- "Max"
  
  # Check to make sure that if users provided some more information
  # about what line and ribbon to plot is available to plot.
  extras <- list(...)
  if (length(extras) > 0) {
    if (!identical(setdiff(names(extras), c("center", "lower", "upper")), character(0))) {
      stop("... can only include center, lower, and upper")
    }
    
    if (!all(c(center, lower, upper) %in% names(x))) {
      stop("center, lower, upper not in data")
    }
    
  }
  
  # Make the data frame of the values to plot
  to_plot <- data.frame(x[center],
                        x[lower],
                        x[upper],
                        x["variable"],
                        x["time"], 
                        x["name"])
  colnames(to_plot) <-
    c("center", "lower", "upper", "variable", "time", "name")
  
  colorpalette <- memc_colorPalette(name = unique(to_plot$name))
  
  ggplot(data = to_plot) +
    geom_line(aes(time, center, color = name)) +
    geom_ribbon(aes(time, ymin = lower, ymax = upper, fill = name), alpha = 0.5) +
    facet_wrap("variable", scales = "free") +
    theme_bw() +
    scale_color_manual(values = colorpalette) + 
    scale_fill_manual(values = colorpalette) +
    theme(legend.title = element_blank()) +
    labs(x = "Time (days)", y = NULL) ->
    out
  
  return(out)
  
}



#' Quick plot the results returned from memc_sensFunc
#'
#'
#' @param x An object of class `memc_sensFunc`.
#' @param y Ignored
#' @param ... additional arguments 
#' @export
#' @import ggplot2
#' @method plot memc_sensFunc
plot.memc_sensFunc <- function(x, y, ...) {
  
  # Silence check warnings
  time <- value <- parameter <- NULL
  
  if (!inherits(x, "memc_sensFunc"))
    stop("Object is not of class 'memc_sensFunc'")

  ggplot(data = x) +
    geom_line(aes(time, value, color = parameter)) +
    facet_wrap("variable", scales = "free") +
    theme_bw() +
    labs(x = "Time (days)", y = "Difference in Output") +
    theme(legend.title = element_blank()) ->
    out
  
  return(out)
  
}


#' Combine results returned from memc_solve
#'
#'
#' @param ... the `memc_solve` objects to be combined into a single data frame
#' @param deparse.level the default deparse.level = 1 typically and deparse.level = 2 always construct labels from the argument names, see the ‘Value’ section below. 
#' @export
#' @method rbind memc_solve
rbind.memc_solve <- function(..., deparse.level = 1) {
  inputs <- list(...)
  
  # Check if any inputs are of the same class to avoid recursion
  if (any(!sapply(inputs, inherits, what = "memc_solve"))) {
    stop("All objects need to be 'memc_solve' class")
  }
  
  # Convert to data frame to make easy to rbind
  data.frame_inputs <- lapply(X = inputs, function(x) {
    class(x) <- "data.frame"
    return(x)
  })
  
  out <- do.call(what = "rbind", args = data.frame_inputs)
  class(out) <- c("memc_solve", class(out))
  return(out)
  
}



#' Combine results returned from memc_sensrange
#'
#'
#' @param ... the `memc_sensrange` objects to be combined into a single data frame
#' @param deparse.level the default deparse.level = 1 typically and deparse.level = 2 always construct labels from the argument names, see the ‘Value’ section below. 
#' @export
#' @method rbind memc_sensrange
rbind.memc_sensrange <- function(..., deparse.level = 1) {
  inputs <- list(...)
  
  # Check if any inputs are of the same class to avoid recursion
  if (any(!sapply(inputs, inherits, what = "memc_sensrange"))) {
    stop("All objects need to be 'memc_sensrange' class")
  }
  
  # Convert to data frame to make easy to rbind
  data.frame_inputs <- lapply(X = inputs, function(x) {
    class(x) <- "data.frame"
    return(x)
  })
  
  out <- do.call(what = "rbind", args = data.frame_inputs)
  class(out) <- c("memc_sensrange", class(out))
  return(out)
  
}
