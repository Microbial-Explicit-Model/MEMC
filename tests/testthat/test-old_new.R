# Read in the comparison data and figure out the time vector.
old <- read.csv("old-new.csv")
t <- unique(old$time)

# Helper function that calculates the difference between two data frame
# Args
#   old: data frame of the old comparison data
#   new: data frame of the new data
# Return: data frame of the new, old, and different data
old_new_diff <- function(old, new) {
  cond <- all(names(old) == c("time", "variable", "old_value", "units", "name"))
  assertthat::assert_that(cond)

  cond <-
    all(names(new) == c("time", "variable", "value", "units", "name"))
  assertthat::assert_that(cond)

  cond <- all(old[["time"]] == new[["time"]])
  assertthat::assert_that(cond)

  cond <- all(old[["variable"]] == new[["variable"]])
  assertthat::assert_that(cond)

  new[["old_value"]] <- old[["old_value"]]
  new[["diff"]] <- old[["old_value"]] - new[["value"]]
  return(new)

}

test_that("MEND behavior", {

  new <- memc_solve(mod = MEMC::MEND_model, time = t)
  old_comp <- old[old$name == "MEND",]
  out <- old_new_diff(old_comp, new)
  expect_true(all(out$diff <= 1e-8))

})

test_that("COMISSION behavior", {
  new <- memc_solve(mod = MEMC::COMISSION_model, time = t)
  old_comp <- old[old$name == "COMISSION",]

  out <- old_new_diff(old_comp, new)

#  expect_true(all(out$diff <= 1e-7))

})

test_that("CORPSE behavior", {
  new <- memc_solve(mod = MEMC::CORPSE_model, time = t)
  old_comp <- old[old$name == "CORPSE",]

  out <- old_new_diff(old_comp, new)

  expect_true(all(out$diff <= 1e-8))

})

test_that("MEMS behavior", {
  new <- memc_solve(mod = MEMC::MEMS_model, time = t)
  old_comp <- old[old$name == "MEMS",]

  out <- old_new_diff(old_comp, new)

  expect_true(all(out$diff <= 1e-8))

})

test_that("MIMCS behavior", {
  new <- memc_solve(mod = MEMC::MIMCS_model, time = t)
  old_comp <- old[old$name == "MIMCS",]

  out <- old_new_diff(old_comp, new)

  expect_true(mean(out$diff) <= 1e-8)

})

test_that("BAMS behavior", {
  new <- memc_solve(mod = MEMC::BAMS_model, time = t)
  old_comp <- old[old$name == "BAMS",]

  out <- old_new_diff(old_comp, new)

  expect_true(mean(abs(out$diff)) <= 1e-8)

})

