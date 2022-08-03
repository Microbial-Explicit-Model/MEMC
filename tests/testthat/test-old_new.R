# Read in the comparison data and figure out the time vector.
old <- read.csv("old-new.csv")
t <- unique(old$time)

# Helper function that calculates the difference between two data frame
# Args
#   old: dataframe of the old comparison data
#   new: dataframe of the new data
# Return: dataframe of the new, old, and different data
old_new_diff <- function(old, new) {
  cond <-
    all(names(old) == c("time", "variable", "old_value", "units", "name"))
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
  new <- solve_model(mod = MEMC::MEND_model, time = t)
  old_comp <- old[old$name == "MEND",]

  out <- old_new_diff(old_comp, new)

  expect_true(all(out$diff <= 1e-8))

})

test_that("COMISSION behavior", {
  new <- solve_model(mod = MEMC::COMISSION_model, time = t)
  old_comp <- old[old$name == "COMISSION",]

  out <- old_new_diff(old_comp, new)

  expect_true(all(out$diff <= 1e-8))

})

test_that("CORPSE behavior", {
  new <- solve_model(mod = MEMC::CORPSE_model, time = t)
  old_comp <- old[old$name == "CORPSE",]

  out <- old_new_diff(old_comp, new)

  expect_true(all(out$diff <= 1e-8))

})
