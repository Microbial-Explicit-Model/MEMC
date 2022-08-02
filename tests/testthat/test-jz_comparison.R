# Compare MEMC output from the different configuration with output from JZ.
# This test is used to check to make sure the model configurations are set up
# correctly.
library(data.table)

# Read in the old comparison data
old <- read.csv("jz_output.csv")

# Determine the time values to use in the comparison
t <- unique(old$time)

# Helper function that calculates the percent change between the output streams
# Args
#   old: data.table of the out jz data
#   new: data.table of the new data
# Returns: data table of the mean difference, the total mean, and the percent of the total mean value
calculate_percent_diff <- function(old, new){

 old <- as.data.table(old)
 new <- as.data.table(new)

 wide <- old[new, nomatch=0, on=c("time", "variable", "name")]
 wide$dif <- abs(wide$jz_value - wide$value)
 out <- wide[ , list("mean_dif" = mean(dif), "mean" = mean(jz_value)), by = c("variable")]
 out$percent <- out$mean_dif/out$mean
 return(out)
}



test_that("old jz mend", {

  out <- solve_model(MEND_model,
                     time = t,
                     state = c(P=4.71, M=17.67, Q=0, B=0.82, D=0.148, EP=0.0082, EM=0.0082, IC=0, Tot=23.484),
                     params = c(V.d=3,V.p=14, V.m=0.25))[["results"]]

  dif_df <- calculate_percent_diff(old = old, new = out)
  expect_true(all(dif_df$percent <= 0.1))

})


test_that("old jz comission", {

  out <- solve_model(COMISSION_model,
                     time = t,
                     state = c(P=4.71, M=17.67, Q=0, B=0.52, D=0.148, EP=0.052, EM=0.052, IC=0, Tot=23.484),
                     params = c(V.d=1,V.p=5, V.m=1))[["results"]]

  dif_df <- calculate_percent_diff(old = old, new = out)
  expect_true(all(dif_df$percent <= 0.1))

})

test_that("old jz corpse", {


  out <- solve_model(CORPSE_model,
                     time = t,
                     state = c(P=4.71, M=17.67, Q=0, B=0.52, D=0.148, EP=0.052, EM=0.052, IC=0, Tot=23.484),
                     params = c(V.d=0.5,V.p=0.001, V.m=0.001))[["results"]]
  dif_df <- calculate_percent_diff(old = old, new = out)
  expect_true(all(dif_df$percent <= 0.3))

})
