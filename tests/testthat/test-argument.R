# Test the functions that check to make sure the inputs to functions are correct!


test_that("is_param_table works", {
  params <- memc_params

  # The default parameter table should always pass this test.
  expect_true(is_param_table(params))

  # A number of errors should be thrown if the the parameter table does not meet the correct conditions.
  expect_error(
    is_param_table(params[1:10, ]),
    "param_table is missing a parameter value(s) for: r_ep, r_em, Q_max, K_ads, K_des, dd_beta, Input_POM, Input_DOM, CUE",
    fixed = TRUE
  )

  ptable_fake_entry <-
    rbind(
      params,
      data.frame(
        parameter = "fake",
        description = "fake",
        units = "fake",
        value = "fake"
      )
    )
  expect_error(
    is_param_table(ptable_fake_entry),
    "param_table contains unkown parameter value(s): fake",
    fixed = TRUE
  )

  expect_error(
    is_param_table(params[, 1:2]),
    "param_table is missing a required column: parameter, description, units, value",
    fixed = TRUE
  )
  expect_error(
    is_param_table(params[, 3:4]),
    "param_table is missing a required column: parameter, description, units, value",
    fixed = TRUE
  )

})


test_that("is_state_vector works", {
  state <- MEMC::memc_initial_state

  # The default initial state vector should always pass this test.
  assert_that(is_state_vector(state))

  # A number of errors should be thrown if the input does not meet the correct requirements.
  expect_error(
    is_state_vector(rev(state)),
    "entries must be in the correct order",
    fixed = TRUE
  )

  names(state) <- NULL
  expect_error(is_state_vector(state),
               "names(state) is not a character vector",
               fixed = TRUE)

  state <- as.character(state)
  names(state) <- names(MEMC::memc_initial_state)
  expect_error(is_state_vector(state),
               "state is not a numeric or integer vector")

})


test_that("is_state_vector", {

  # If there is a missing initial state value throw an error.
  state <- c(POM = 4.71, MOM = 17.67, QOM = 0, MB = 0.52, DOM = 0.148,
             EM = 0.052, IC = 0, Tot = 23.484)
  expect_error(is_state_vector(state), label = "state is missing a value(s) for: EP")

  # Make sure the pattern is in the correct order!
  state <- c(POM = 4.71, MOM = 17.67, DOM = 0.148, QOM = 0, MB = 0.52,
             EP = 0.052, EM = 0.052, IC = 0, Tot = 23.484)
  expect_error(is_state_vector(state), "entries must be in the correct order")

})


test_that("is_memc_config", {

    # All the models should pass
    for(m in memc_all_models) {
        expect_true(is_memc_config(m))
    }

    # Something completely different should fail
    expect_false(is_memc_config(cars))

    # Test should be sensitive to object names, types, and length
    m <- MEMS_model
    expect_true(is_memc_config(m))

    names(m)[1] <- "wrong name" # change name
    expect_false(is_memc_config(m))

    m <- MEMS_model
    m[[1]] <- cars # change object
    expect_false(is_memc_config(m))

    m <- c(MEMS_model, list(x = 1)) # add object at end
    expect_false(is_memc_config(m))

    # However, the order of objects shouldn't matter
    m <- MEMS_model
    x <- m[[1]]
    x_name <- names(m)[1]
    m[[1]] <- m[[2]]
    names(m)[1] <- names(m)[2]
    m[[2]] <- x
    names(m)[2] <- x_name
    expect_true(is_memc_config(m))
})
