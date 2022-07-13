# this really needs to be expanded upon! but for now making sure that
# running under different conditions has an affect on the output

test_that("change inputs", {

  time <- seq(0, 100, length.out = 10)
  config <- list("name" = "test",
                 "table" = NULL,
                 "params" = MEMC::default_params,
                 "state" = MEMC::default_initial,
                 "derivs" = MEMC::derivs)

  mend_out1 <- solve_model(mod = config, time)

  # Start by chaning an inital value, these tests are not that strict...
  mend_out2 <- solve_model(mod = config, time, state = c(B = 4))

  expect_equal(sum(abs(mend_out1$model$state - mend_out2$model$state) > 0), 1)
  expect_true(sum(abs(mend_out1$results$value - mend_out2$results$value) > 1e-3) > 4)


  mend_out3 <- solve_model(mod = config, time, params = c("V.d" = 10))
  expect_equal(mean(mend_out1$model$state - mend_out3$model$state), 0)
  expect_equal(sum(abs(mend_out1$model$params$value - mend_out3$model$params$value) > 0), 1)
  expect_true(sum(abs(mend_out1$results$value - mend_out3$results$value) > 1e-3) > 4)


})
