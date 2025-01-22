ptable <- memc_params
state <- memc_initial_state

test_that("update_params", {
  # Test that it works
  new_table <-
    memc_update_params(c(
      "V_p" = 10,
      "K_d" = 10,
      "Input_POM" = 10
    ), param_table = ptable)
  expect_equal(sum(abs(new_table$value - memc_params$value) > 0), 3)
  
  # Expect an error if conditions aren't met
  expect_error(
    memc_update_params(c(
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
  expect_equal(sum(abs(new_state - MEMC::memc_initial_state) > 0), 1)
  
  # Expect an error if conditions aren't met
  expect_error(update_state(c("fake" = 3), state = state))
})

test_that("bad model configuration will fail", {
  out1 <- memc_configure(ptable, state)
  expect_equivalent(class(out1), "memc_single_config")
  
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
  expect_error(memc_configure(ptable, state, MBdecay = "fake"),
               'MBdecay must be "LM" or "DD"',
               fixed = TRUE)
  
  # Only change one parameter value and one state value
  new_out <-
    memc_update_config(mod = out1, new = c("V_d" = 50, "MB" = 50))
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

test_that("memc_colorPalette ", {
  out <- memc_colorPalette()
  expect_vector(out)
  expect_equal(length(out), length(memc_all_configs))
  expect_equal(length(memc_colorPalette("MEND")), 1)
  expect_error(memc_colorPalette("fake"))
  
})

test_that("custom summary and print functions work as expected", {
  # Confirm that the class of the memc_all_configs object
  expect_s3_class(memc_all_configs, "memc_all_configs")
  
  # Check to make sure that our custom summary function returns what we
  # are expecting it to
  x <- summary(memc_all_configs)
  expect_s3_class(x, "knitr_kable")
  
  # But when printing the memc_all_configs the users should
  # not see the custom class
  x <- capture.output(class(print(memc_all_configs)))
  expect_equivalent(tail(x, n = 1), "[1] \"list\"")
  
  
  # Confirm that the class of the a single model object
  expect_s3_class(MEND_config, "memc_single_config")
  
  # Check to make sure that the custom summary and print function return
  # what we are expecting it to
  x <- summary(MEND_config)
  expect_s3_class(x, "knitr_kable")
  
  x <- capture.output(class(print(MEND_config)))
  expect_equivalent(tail(x, n = 1), "[1] \"list\"")
  
  
})
