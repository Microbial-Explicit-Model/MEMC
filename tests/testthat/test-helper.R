#TODDO might not want to hard code these, might want to doubble or make the changes relativev


test_that("update_params", {

  new_table <- update_params(c("V.p" = 10, "K.d" = 10, Input = 10), param_table = MEMC::default_params)
  expect_equal(sum(abs(new_table$value - MEMC::default_params$value) > 0), 3)

  expect_error(update_params(c("fake" = 10, "K.d" = 10, Input = 10), param_table = MEMC::default_params),
               "new_params must refer to a parameter already existing in param_table \n the following params are not recognized: fake", fixed = TRUE)

})

test_that("update_state", {

  new_state <- update_state(new_vals = c("Q" = 10), state = MEMC::default_initial)
  expect_equal(sum(abs(new_state - MEMC::default_initial) > 0), 1)

  expect_error(update_state(c("ffake" = 3), state = MEMC::default_initial))
})
