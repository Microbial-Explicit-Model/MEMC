# The purpose of this test is to ensure that with minor package development there is no
# change in the output behavior. When there are major developmental changes made to the
# package the comparison data might have to be updated.
context('old new tests')

# Read in the archived comparison data.
old <- read.csv('compdata/comp.csv')

t <- unique(old$time)

# MEND varaitions -------------------------------------------------------------------------
testthat::test_that("MEND", {
  # Set up and solve MEND two different ways, with the internal functions and
  # with the MEND wrapper.
  out1 <- solver(params = MEND_params,
                time = t,
                state = MEND_initalState,
                carbon_pools_func = MEND_pools,
                carbon_fluxes_func = MEND_fluxes)

  out2 <- MEND(parameters = MEND_params, time = t, inital_state = MEND_initalState)
  expect_equal(out2, out1)

  # Compare output with the archived output.
  old <- old[old$name == "MEND", ]
  expect_equal(old$value, out1$value)
})

testthat::test_that("MEND_RM", {

  out1 <- MEND_RM(parameters = MEND_params, time = t, inital_state = MEND_initalState)
  comp_data <- old[old$name == "MEND_RM", ]
  expect_equal(comp_data$value, out1$value)

})

testthat::test_that("MEND_ECA", {

  out1 <- MEND_ECA(parameters = MEND_params, time = t, inital_state = MEND_initalState)
  comp_data <- old[old$name == "MEND_ECA", ]
  expect_equal(comp_data$value, out1$value)

})

testthat::test_that("MEND_MILLEN", {

  out1 <- MEND_MILLEN(parameters = MEND_params, time = t, inital_state = MEND_initalState)
  comp_data <- old[old$name == "MEND_MILLEN", ]
  expect_equal(comp_data$value, out1$value)

})


# MEND2 varaitions -------------------------------------------------------------------------
testthat::test_that("MEND2", {

  out1 <- MEND2(parameters = MEND2_params, time = t, inital_state = MEND2_initalState)
  comp_data <- old[old$name == "MEND2", ]
  expect_equal(comp_data$value, out1$value)

})

testthat::test_that("MEND2_RM", {

  out1 <- MEND2_RM(parameters = MEND2_params, time = t, inital_state = MEND2_initalState)
  comp_data <- old[old$name == "MEND2_RM", ]
  expect_equal(comp_data$value, out1$value)

})

testthat::test_that("MEND2_ECA", {

  out1 <- MEND2_ECA(parameters = MEND2_params, time = t, inital_state = MEND2_initalState)
  comp_data <- old[old$name == "MEND2_ECA", ]
  expect_equal(comp_data$value, out1$value)

})

testthat::test_that("MEND2_MILLEN", {

  out1 <- MEND2_MILLEN(parameters = MEND2_params, time = t, inital_state = MEND2_initalState)
  comp_data <- old[old$name == "MEND2_MILLEN", ]
  expect_equal(comp_data$value, out1$value)

})
