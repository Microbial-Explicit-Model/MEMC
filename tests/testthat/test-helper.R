#TODDO might not want to hard code these, might want to doubble or make the changes relativev

ptable <- MEMC::default_params
state <- MEMC::default_initial

test_that("update_params", {

  # Test that it works
  new_table <- update_params(c("V.p" = 10, "K.d" = 10, Input = 10), param_table = ptable)
  expect_equal(sum(abs(new_table$value - MEMC::default_params$value) > 0), 3)

  # Expect an error if conditions aren't met
  expect_error(update_params(c("fake" = 10, "K.d" = 10, Input = 10), param_table = ptable),
               "new_params must refer to a parameter already existing in param_table \n the following params are not recognized: fake", fixed = TRUE)

})

test_that("update_state", {

  # Test that it works
  new_state <- update_state(new_vals = c("Q" = 10), state = state)
  expect_equal(sum(abs(new_state - MEMC::default_initial) > 0), 1)

  # Expect an error if conditions aren't met
  expect_error(update_state(c("fake" = 3), state = state))
})

test_that("configuration related fxns", {

  out1 <- configure_model(ptable, state)
  out2 <- configure_model(ptable, state, DOMdecomp = "RMM", POMdecomp = "ECA", MBdecay = "LM")

  # Expect changes to be made to the dynamics table
  expect_true(out1[["table"]][["DOMdecomp"]] != out2[["table"]][["DOMdecomp"]])
  expect_true(out1[["table"]][["POMdecomp"]] != out2[["table"]][["POMdecomp"]])
  expect_true(out1[["table"]][["MBdecay"]] != out2[["table"]][["MBdecay"]])

  # Errors should be thrown
  expect_error(configure_model(ptable[1:9, ], state), 'param_table is missing a parameter value(s) for: g.d, p.ep, p.em, r.ep, r.em, Q.max, K.ads, K.des, dd.beta, Input, CUE', fixed = TRUE)
  expect_error(configure_model(ptable, state, DOMdecomp = "fake"), 'DOMdecomp must be "MM", "RMM", "ECA"', fixed = TRUE)
  expect_error(configure_model(ptable, state, POMdecomp = "fake"), 'POMdecomp must be "MM", "RMM", "ECA", "LM"', fixed = TRUE)
  expect_error( configure_model(ptable, state, MBdecay = "fake"), 'MBdecay must be "LM" or "DD"', fixed = TRUE)

  # Only change one parameter value and one state value
  new_out <- update_config(out1, params = c("V.d" = 50), state = c("B" = 50))
  expect_equal(sum(new_out$params$value != out1$params$value), 1)
  expect_equal(sum(new_out$state != out1$state), 1)


})

test_that("split_param_state", {

  out <- split_param_state(x = c("V.d" = 10))
  expect_equal(length(out), 2)
  expect_true(is.numeric(out$params))
  expect_null(out$state)

  out <- split_param_state(x = c("B" = 10))
  expect_equal(length(out), 2)
  expect_true(is.numeric(out$state))
  expect_null(out$params)

  out <- split_param_state(x = c("B" = 10, "V.d" = 10))
  expect_equal(length(out), 2)
  expect_equal(length(out$params), 1)
  expect_equal(names(out$params), "V.d")
  expect_equal(names(out$state), "B")

  expect_error(split_param_state(x = c("fake" = 10)), "value not recognized as a parameter or state")

})
