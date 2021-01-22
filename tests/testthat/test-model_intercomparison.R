context('model_intercomparison')


test_that("model_intercomparison", {

  out <- model_intercomparison(MEND2013_params, t = 1:10, MEND2013_initalState)
  expect_true(data.table::is.data.table(out))

  # Make sure that the models return different results from one another.
  rslts <- data.table::dcast(out, units + variable + time ~ model, value.var = 'value')
  expect_true(sum(abs(rslts$MEND2013 - rslts$MEND2013_RM)) > 0)

})

