
library(dplyr)

test_that("old jz mend", {

  config <- list("name" = "test",
                 "table" = NULL,
                 "params" = MEMC::default_params,
                 "state" = c(P=4.71, M=17.67, Q=0, B=0.82, D=0.148,  EP=0.0082, EM=0.0082, IC=0, Tot=23.484),
                 "derivs" = MEMC::derivs)

  jz_out <- read.csv("jz_mend.csv")
  names(jz_out) <- c("time", "variable", "jz_value", "units")

  mend_out <- solve_model(mod = config, unique(jz_out$time))[["results"]]
  inner_join(jz_out, mend_out) %>%
    mutate(dif = abs(jz_value - value)) %>%
    group_by(variable) %>%
    summarise(mean_abs = mean(dif)) %>%
    ungroup %>%
    pull(mean_abs) ->
    vals

  expect_true(all(vals <= 1e-8))

})
