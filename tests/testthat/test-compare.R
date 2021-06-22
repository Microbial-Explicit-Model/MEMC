# Test the comparison functions.

# Define the time steps to solve the model at.
t <- seq(0, 100, by = 20)


test_that("compare_MEND:", {

  out <- compare_MEND(MEND_params, t, MEND_initalState)
  expect_equal(length(unique(out$model)), 4)

})

test_that("compare_MEND2:", {

  out <- compare_MEND2(MEND2_params, t, MEND2_initalState)
  expect_equal(length(unique(out$model)), 4)

})
