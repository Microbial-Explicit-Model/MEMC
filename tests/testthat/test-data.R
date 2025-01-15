test_that("incubation data", {
  
  expect_identical(class(memc_incubation), "list")
  expect_equal(length(memc_incubation), 4)
  expect_identical(class(memc_incubation$ultisol$data), "data.frame")
  expect_identical(class(memc_incubation$ultisol$state), "numeric")

})




