# This test preserves the model configuration during model development
# eventually this will be the offical old new test the
library(dplyr)
test_that("old new mend behavior", {

  config <- list("name" = "test",
                 "table" = NULL,
                 "params" = MEMC::default_params,
                 "state" = MEMC::default_initial,
                 "derivs" = MEMC::derivs)

  old_out <- read.csv("old_mend.csv")

  mend_out <- solve_model(mod = config, time = unique(old_out$time))[["results"]]
  inner_join(old_out, mend_out) %>%
    mutate(dif = abs(old_value - value)) %>%
    group_by(variable) %>%
    summarise(mean_abs = mean(dif)) %>%
    ungroup %>%
    pull(mean_abs) ->
    vals

  expect_true(all(vals <= 1e-8))
})
