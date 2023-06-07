# Compare out package outputs with the results from JZ, this is a proof of concept that the pacakge
# is working as expected.

library(data.table)
library(dplyr)
library(ggplot2)

tol <- 1e-6

jz_initial_conditions <- c(P = 4.71, M = 17.67, Q = 0, B = 0.82, D = 0.148, EP = 0.0082, EM = 0.0082, IC = 0, Tot = 23.484)

params <- c("V_p", "K_p", "V_m", "K_m", "V_d", "K_d", "f_d", "g_d", "p_ep", "p_em", "r_ep", "r_em", "Q_max", "K_ads",
            "K_des", "dd_beta", "Input_P", "Input_D", "Input_M", "CUE")
desc <- c("maximum specific decomposition rate for P by EP", "half-saturation constant for decomposition of P", "maximum specific decomposition rate for M by EM",
          "half-saturation constant for decomposition of M", "maximum specific uptake rate of D for growth of B", "half-saturation constant of uptake of D for growth of B",
          "fraction of decomposed P allocated to D", "fraction of dead B allocated to D", "fraction of mR for production of EP", "fraction of mR for production of EM",
          "turnover rate of EP", "turnover rate of EM", "maximum DOC sorption capacity", "specific adsorption rate", "desorption rate", "strength of density dependent microbial decay",
          "POM input", "DOM input", "M input", "carbon use efficiency")
value <- c("V_p" = 14, "K_p" = 50, "V_m" = 0.25, "K_m" = 250, "V_d" = 3, "K_d"= 0.25, "f_d" = 0.5, "g_d" = 0.5, "p_ep" = 0.01, "p_em" = 0.01, "r_ep" = 1e-3,
           "r_em" = 1e-3 , "Q_max" = 3.4, "K_ads" = 0.006, "K_des"= 0.001, "dd_beta" = 2, "Input_P" = 0, "Input_D" = 0, "Input_M" = 0, "CUE" = 0.4)
units <- c("mgC mgC^-1 h^-1", "mgC / g soil", "mgC mgC^-1 h^-1", "mg C/g soil", "mgC mgC^-1 h^-1", "mg C/g soil", NA,
           NA, NA, NA, "mgC mgC^-1 h^-1", "mgC mgC^-1 h^-1", "mgC / g soil", "mgC mgC^-1 h^-1", "mgC mgC^-1 h^-1", NA,
           "mg C", "mg C", "mg C", "")

param_dt <- data.table("parameter" = params, "description" = desc, "units" = units, "value" = value)
times <- 0:36500



test_that("MEND", {

  # Set up the mend model and run results
  mod <- configure_model(params = param_dt,
                         state = jz_initial_conditions,
                         name = "JZ mend",
                         DOMdecomp = "MM", POMdecomp = "MM" , MBdecay = "DD")
  out_JZ_mend <- solve_model(mod = mod, time = times)

  jz_results <- data.table(read.csv("mend_jz.csv"))
  names(jz_results) <- c("time", "variable", "jz_value", "units")

  # Compare the results!
  joined_df <- out_JZ_mend[jz_results, on = c("time", "variable", "units")]
  expect_equal(joined_df$value, joined_df$jz_value, tolerance = tol)

})


test_that("COM", {


  state<-c(P=4.71, M=17.67, Q=0, D=0.148, B=0.52, EP=0.052, EM=0.052, IC=0, Tot=23.484)#Ultisol

  #jz_initial_conditions <- c(P = 4.71, M = 17.67, Q = 0, B = 0.52, D = 0.148, EP = 0.052, EM = 0.052, IC = 0, Tot = 23.484)


  # Most of parameters were defined the same way, update the ones that are different.
  params_to_use <- update_params(new_params = c("V_d" = 1, "V_p" = 5, "V_m" = 1), param_table = param_dt)

  # Set up the mend model and run results
  mod <- configure_model(params = params_to_use,
                         state = state,
                         name = "JZ com",
                         DOMdecomp = "MM", POMdecomp = "RMM" , MBdecay = "DD")
  out_JZ_com <- solve_model(mod = mod, time = times)


  jz_results <- data.table(read.csv("com_jz.csv"))
  names(jz_results) <- c("time", "variable", "jz_value", "units")

  # Compare the results!
  joined_df <- out_JZ_com[jz_results, on = c("time", "variable", "units")]


  joined_df %>%
    mutate(dif = value - jz_value) %>%
    filter(dif > tol)

  expect_equal(joined_df$value, joined_df$jz_value, tolerance = tol)

  joined_df %>%
    ggplot() +
    geom_line(aes(time, value, color = "mine")) +
    geom_line(aes(time, jz_value, color = "jz")) +
    facet_wrap("variable", scales = "free")



})
