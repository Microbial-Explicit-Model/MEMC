test_that("incubation data", {

    expect_is(memc_incubation_andisol, "list")
    expect_identical(length(memc_incubation_andisol), 2L)
    expect_is(memc_incubation_andisol$data, "data.frame")
    expect_is(memc_incubation_andisol$state, "numeric")

    expect_is(memc_incubation_gelisol, "list")
    expect_identical(length(memc_incubation_gelisol), 2L)
    expect_is(memc_incubation_gelisol$data, "data.frame")
    expect_is(memc_incubation_gelisol$state, "numeric")

    expect_is(memc_incubation_mollisol, "list")
    expect_identical(length(memc_incubation_mollisol), 2L)
    expect_is(memc_incubation_mollisol$data, "data.frame")
    expect_is(memc_incubation_mollisol$state, "numeric")

    expect_is(memc_incubation_ultisol, "list")
    expect_identical(length(memc_incubation_ultisol), 2L)
    expect_is(memc_incubation_ultisol$data, "data.frame")
    expect_is(memc_incubation_ultisol$state, "numeric")

})




