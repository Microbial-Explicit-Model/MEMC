# Read in the archived comparison data.
old <- read.csv('compdata/com-new.csv')
t <- unique(old$time)

test_that("MEND_model behavivor is preserved", {

  mend_out <- solve_model(mod = MEND_model, time = t)
  old_mend <- old[old["name"] == "MEND", ]
  compdf <- merge(mend_out, old_mend, by = c("time", "variable", "units", "name"))
  expect_equal(compdf$value, compdf$old_value)

})

test_that("COMISSION_model behavivor is preserved", {

  comissions_out <- solve_model(mod = COMISSION_model, time = t)
  old_df<- old[old["name"] == "COMISSION", ]
  compdf <- merge(comissions_out, old_df, by = c("time", "variable", "units", "name"))
  expect_equal(compdf$value, compdf$old_value)

})

test_that("CORPSE_model behavivor is preserved", {

  corpse_out <- solve_model(mod = CORPSE_model, time = t)
  old_df<- old[old["name"] == "CORPSE", ]
  compdf <- merge(corpse_out, old_df, by = c("time", "variable", "units", "name"))
  expect_equal(compdf$value, compdf$old_value)

})


