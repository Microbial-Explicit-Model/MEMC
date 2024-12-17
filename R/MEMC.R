#' @details This package allows you run and manipulate the kinetics used
#' in MEND (Wang et al. 2015) a popular microbial-explicit eight carbon
#' pool model enabling users to explore how changing the formulation of
#' microbial activity affects soil carbon pool dynamics.
#'
#' @section MEMC model configurations: The MEMC package allows users to
#'   build their own models using \code{\link{configure_model}} but also
#'   contains several ready to go model configurations such as
#'   \code{\link{MEND_model}} and \code{\link{COMISSION_model}}.
#'
#' @section Getting started: To run a MEMC model configuration use
#'   \code{\link{memc_solve}} with one of the pre configured MEMC
#'   models \code{\link{memc_solve}} and \code{\link{COMISSION_model}}.
#'   Using the \code{\link{memc_solve}} function with the pre-configured
#'   package models, you can read in your own model parameter table or
#'   initial state values.
#'
#' @section Advanced usage: You can configure your own toy model using
#'   \code{\link{configure_model}} with \code{\link{carbon_pools}} and
#'   \code{\link{carbon_fluxes}}. The function
#'   \code{\link{modify_fluxes_func}} allows users to customize the
#'   microbial kinetics.
