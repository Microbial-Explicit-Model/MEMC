# Check to make sure that the various model configurations that are included by default
# are valid MEMC objects in other words we are able to solve them using the solve_model
# function. Testing to make sure that the make configure_model function works & throws
# errors as expected happens in the test-helper.R script.
t <- 1:10

test_that("model configurations solve", {
  mend_out <- solve_model(MEND_model, time = t)
  expect_true(is.data.frame(mend_out))

  out <- solve_model(COMISSION_model, time = t)
  expect_true(is.data.frame(out))

  out <- solve_model(CORPSE_model, time = t)
  expect_true(is.data.frame(out))

})
