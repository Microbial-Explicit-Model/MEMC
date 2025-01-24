test_that("class memc_all_configs", {
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
  
})

test_that("class memc_single_config", {
  # Confirm that the class of the a single model object
  expect_s3_class(MEND_config, "memc_single_config")
  
  # Check to make sure that the custom summary and print function return
  # what we are expecting it to
  x <- summary(MEND_config)
  expect_s3_class(x, "knitr_kable")
  
  x <- capture.output(class(print(MEND_config)))
  expect_equivalent(tail(x, n = 1), "[1] \"list\"")
  
})

test_that("class memc_solve", {
  out1 <- memc_solve(MEND_config, time = 0:10)
  expect_s3_class(out1, "memc_solve")
  
  # Plot works on a single result from memc_solve
  expect_equivalent(class(plot(out1)), c("gg", "ggplot"))
  
  # rbind works and returns on multiple objects that can be plotted
  out2 <- memc_solve(MIMCS_config, time = 0:10)
  out <- rbind(out1, out2)
  expect_equal(length(unique(out$name)), 2)
  expect_equivalent(class(plot(out)), c("gg", "ggplot"))
  
})

test_that("class memc_sensrange ", {
  
  # Set up the parameter values to test and use in the sensrange.
  frac <- 0.5
  n <- 3
  p <- c("V_p" = 1.4e+01)
  prange <- data.frame(min = p - p * frac, max = p + p * frac)
  
  # Run the sense range and make sure the objects we want are returned
  out <-
    memc_sensrange(
      MEND_config,
      t = 1:10,
      x = p,
      n = n,
      dist = "latin",
      parRange = prange
    )
  
  expect_s3_class(out, c("memc_sensRange", "data.frame"))
  expect_equivalent(class(plot(out)), c("gg", "ggplot"))
  
  # Make sure no error is thrown when the ribbons are changed.
  works <- plot(out, lower = "q5", upper = "q90")
  expect_equivalent(class(works), c("gg", "ggplot"))
  
  # Change the number itterations to run
  out2 <-
    memc_sensrange(
      MEND_config,
      t = 1:10,
      x = p,
      n = 2,
      dist = "latin",
      parRange = prange
    )
  expect_s3_class(rbind(out, out2) , c("memc_sensRange", "data.frame"))

})


test_that("class memc_sensrange ", {
  
  # Test that the function runs and returns the expected output that
  # can be plotted.
  pars <- c("V_d" = 3.0e+00,
            "V_p" = 1.4e+01,
            "V_m" = 2.5e-01)
  out <- memc_sensfunc(config = MEND_config, t = 1:10, x = pars)
  expect_s3_class(out, c("memc_sensFunc", "data.table", "data.frame"))
  expect_equal(class(plot(out)), c("gg", "ggplot"))
  
})
