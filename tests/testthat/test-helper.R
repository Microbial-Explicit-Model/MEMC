ptable <- MEMC::default_params
state <- MEMC::default_initial

test_that("update_params", {
  # Test that it works
  new_table <-
    update_params(c(
      "V_p" = 10,
      "K_d" = 10,
      "Input_POM" = 10
    ), param_table = ptable)
  expect_equal(sum(abs(
    new_table$value - MEMC::default_params$value
  ) > 0), 3)

  # Expect an error if conditions aren't met
  expect_error(
    update_params(c(
      "fake" = 10,
      "K_d" = 10,
      "Input_POM" = 10
    ), param_table = ptable),
    "new_params must refer to a parameter already existing in param_table \n the following params are not recognized: fake",
    fixed = TRUE
  )

})

test_that("update_state", {
  # Test that it works
  new_state <- update_state(new_vals = c("QOM" = 10), state = state)
  expect_equal(sum(abs(new_state - MEMC::default_initial) > 0), 1)

  # Expect an error if conditions aren't met
  expect_error(update_state(c("fake" = 3), state = state))
})

test_that("bad model configuration will fail", {
  out1 <- memc_configure(ptable, state)
  out2 <-
    memc_configure(
      ptable,
      state,
      DOMuptake = "RMM",
      POMdecomp = "ECA",
      MBdecay = "DD"
    )

  # Expect changes to be made to the dynamics table
  expect_true(out1[["table"]][["DOMuptake"]] != out2[["table"]][["DOMuptake"]])
  expect_true(out1[["table"]][["POMdecomp"]] != out2[["table"]][["POMdecomp"]])
  expect_true(out1[["table"]][["MBdecay"]] != out2[["table"]][["MBdecay"]])

  # Errors should be thrown
  expect_error(
    memc_configure(ptable[1:9,], state),
   "param_table is missing a parameter value(s) for: p_em, r_ep, r_em, Q_max, K_ads, K_des, dd_beta, Input_POM, Input_DOM, CUE",
    fixed = TRUE
  )
  expect_error(
    memc_configure(ptable, state, DOMuptake = "fake"),
    'DOMuptake must be "MM", "RMM", "ECA"',
    fixed = TRUE
  )
  expect_error(
    memc_configure(ptable, state, POMdecomp = "fake"),
    'POMdecomp must be "MM", "RMM", "ECA", "LM"',
    fixed = TRUE
  )
  expect_error(
    memc_configure(ptable, state, MBdecay = "fake"),
    'MBdecay must be "LM" or "DD"',
    fixed = TRUE
  )

  # Only change one parameter value and one state value
  new_out <- update_config(mod = out1, new = c("V_d" = 50, "MB" = 50))
  expect_equal(sum(new_out$params$value != out1$params$value), 1)
  expect_equal(sum(new_out$state != out1$state), 1)


})

test_that("split_param_state", {
  out <- split_param_state(x = c("V_d" = 10))
  expect_equal(length(out), 2)
  expect_true(is.numeric(out$params))
  expect_null(out$state)

  out <- split_param_state(x = c("MB" = 10))
  expect_equal(length(out), 2)
  expect_true(is.numeric(out$state))
  expect_null(out$params)

  out <- split_param_state(x = c("MB" = 10, "V_d" = 10))
  expect_equal(length(out), 2)
  expect_equal(length(out$params), 1)
  expect_equal(names(out$params), "V_d")
  expect_equal(names(out$state), "MB")

  expect_error(split_param_state(x = c("fake" = 10)),
               "value not recognized as a parameter or state")

})


test_that("colorMEMCPalette ", {

  out <- colorMEMCPalette()
  expect_vector(out)
  expect_equal(length(out), nrow(model_configs))
  expect_equal(length(colorMEMCPalette("MEND")), 1)
  expect_error(colorMEMCPalette("fake"))

})
