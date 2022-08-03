# this really needs to be expanded upon! but for now making sure that
# running under different conditions has an affect on the output
ptable <- MEMC::default_params
state <- MEMC::default_initial
time <- seq(0, 1000, length.out = 10)

test_that("change inputs", {
  config <- configure_model(params = ptable,
                            state = state)

  mend_out1 <- solve_model(mod = config, time)

  # Start by changing an initial value, these tests are not that strict...
  mend_out2 <- solve_model(mod = config, time, state = c(B = 4))

  expect_true(sum(abs(mend_out1$value - mend_out2$value) > 1e-3) > 4)


  mend_out3 <-
    solve_model(mod = config, time, params = c("V.d" = 10))
  expect_true(sum(abs(mend_out1$value - mend_out3$value) > 1e-3) > 4)


})


test_that("change model configuration", {
  # The default model configuration
  config <- configure_model(params = ptable, state = state)
  out1 <- solve_model(mod = config, time)

  # Test how changing the DOMdecomp dynamics affects output.
  config <-
    configure_model(params = ptable,
                    state = state,
                    DOMdecomp = "RMM")
  out_alt2 <- solve_model(mod = config, time)
  expect_gt(mean(abs(out1$value - out_alt2$value)), 1e-8)

  config <-
    configure_model(params = ptable,
                    state = state,
                    DOMdecomp = "ECA")
  out_alt3 <- solve_model(mod = config, time)
  expect_gt(mean(abs(out1$value - out_alt3$value)), 1e-8)


  # Test how changing the POMdecomp dynamics affects output.
  config <-
    configure_model(params = ptable,
                    state = state,
                    POMdecomp = "RMM")
  out_alt4 <- solve_model(mod = config, time)
  expect_gt(mean(abs(out1$value - out_alt4$value)), 1e-8)

  config <-
    configure_model(params = ptable,
                    state = state,
                    POMdecomp = "LM")
  out_alt5 <- solve_model(mod = config, time)
  expect_gt(mean(abs(out1$value - out_alt5$value)), 1e-8)

  config <-
    configure_model(params = ptable,
                    state = state,
                    POMdecomp = "ECA")
  out_alt6 <- solve_model(mod = config, time)
  expect_gt(mean(abs(out1$value - out_alt6$value)), 1e-8)

  # Test how changing the MBdecay dynamics affects output.
  config <-
    configure_model(params = ptable,
                    state = state,
                    MBdecay = "LM")
  expect_error(solve_model(mod = config, time), "dd.beta not equal to 1")

  out_alt7 <-
    solve_model(mod = config,
                params = c("dd.beta" = 1),
                time = time)
  expect_gt(mean(abs(out1$value - out_alt7$value)), 1e-8)

})
