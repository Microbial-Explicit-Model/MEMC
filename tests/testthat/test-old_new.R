# TODO remove the dplyr dependency
library(dplyr)

test_that("internal package data is consistent", {

  # Make sure that the internal package data is stable & consistent with the entries in the data-raw.
  # Start with the state values
  state <- MEMC::default_initial

  hard_coded <- c(10.00000, 5.00000, 0.10000, 2.00000, 1.00000, 0.00001, 0.00001, 0.00000, 18.10002)
  names(hard_coded) <- c("P", "M", "Q", "B", "D", "EP", "EM", "IC", "Tot")

  expect_true(all(names(state) == names(hard_coded)))
  MSE <- mean(state - hard_coded)^2
  expect_lte(MSE, 0)


  # Now check to see if the parameter values are
  ptable <- MEMC::default_params
  out <-  read.csv("../../data-raw/default_params.csv")
  expect_equal(out$value, ptable$value)
  expect_equal(out$parameter, ptable$parameter)

})


test_that("old new mend behavior", {

  old_out <- read.csv("old_mend.csv")

  t <- unique(old_out$time)
  config <- configure_model(params = MEMC::default_params,
                            state = MEMC::default_initial)

  mend_out <- solve_model(mod = config, time = t)[["results"]]

  inner_join(old_out, mend_out) %>%
    mutate(dif = abs(old_value - value)) %>%
    group_by(variable) %>%
    summarise(mean_abs = mean(dif)) %>%
    ungroup %>%
    pull(mean_abs) ->
    vals

  expect_true(all(vals <= 1e-8))
})
