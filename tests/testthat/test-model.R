# this really needs to be expanded upon! but for now making sure that
# running under different conditions has an affect on the output
ptable <- MEMC::default_params
state <- MEMC::default_initial

test_that("change inputs", {

  time <- seq(0, 100, length.out = 10)
  config <- configure_model(params = ptable,
                            state = state)

  mend_out1 <- solve_model(mod = config, time)

  # Start by changing an initial value, these tests are not that strict...
  mend_out2 <- solve_model(mod = config, time, state = c(B = 4))

  expect_equal(sum(abs(mend_out1$model$state - mend_out2$model$state) > 0), 1)
  expect_true(sum(abs(mend_out1$results$value - mend_out2$results$value) > 1e-3) > 4)


  mend_out3 <- solve_model(mod = config, time, params = c("V.d" = 10))
  expect_equal(mean(mend_out1$model$state - mend_out3$model$state), 0)
  expect_equal(sum(abs(mend_out1$model$params$value - mend_out3$model$params$value) > 0), 1)
  expect_true(sum(abs(mend_out1$results$value - mend_out3$results$value) > 1e-3) > 4)


})


test_that("change model configuration", {

  config <- configure_model(params = ptable, state = state)
  out1 <- solve_model(mod = config, time)

  config <- configure_model(params = ptable, state = state, DOMdecomp = "RMM")
  out2 <- solve_model(mod = config, time)
  expect_gt(mean(abs(out1$results$value - out2$results$value)), 0)

  config <- configure_model(params = ptable, state = state, POMdecomp = "RMM")
  out3 <- solve_model(mod = config, time)
  expect_gt(mean(abs(out1$results$value - out3$results$value)), 0)

  config <- configure_model(params = ptable, state = state,  MBdecay = "LM")
  expect_error(solve_model(mod = config, time), "dd.beta not equal to 1")

  out4 <- solve_model(mod = config, params = c("dd.beta" = 1), time = time)
  expect_gt(mean(abs(out1$results$value - out4$results$value)), 0)

})
