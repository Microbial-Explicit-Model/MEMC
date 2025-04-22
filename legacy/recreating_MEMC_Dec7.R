# Recreating MEMC_Dec7 with the package
library(data.table)
library(ggplot2)

# Load the MEMC package
devtools::load_all()


jz_initial_conditions <- c(P = 4.71, M = 17.67, Q = 0, B = 0.82, D = 0.148, EP = 0.0082, EM = 0.0082, IC = 0, Tot=23.484)

params <- c("V_p", "K_p", "V_m", "K_m", "V_d", "K_d", "f_d", "g_d", "p_ep", "p_em", "r_ep", "r_em", "Q_max", "K_ads", "K_des", "dd_beta", "Input_P", "Input_D", "Input_M", "CUE")
desc <- c("maximum specific decomposition rate for P by EP", "half-saturation constant for decomposition of P", "maximum specific decomposition rate for M by EM",
          "half-saturation constant for decomposition of M", "maximum specific uptake rate of D for growth of B", "half-saturation constant of uptake of D for growth of B",
          "fraction of decomposed P allocated to D", "fraction of dead B allocated to D", "fraction of mR for production of EP", "fraction of mR for production of EM",
          "turnover rate of EP", "turnover rate of EM", "maximum DOC sorption capacity", "specific adsorption rate", "desorption rate", "strength of density dependent microbial decay",
          "POM input", "DOM input", "M input", "carbon use efficiency")
value <- c("V_p"=14, "K_p"=50, "V_m"=0.25, "K_m"=250, "V_d"=3, "K_d"=0.25, "f_d"=0.5, "g_d"=0.5, "p_ep"=0.01, "p_em"=0.01, "r_ep"=1e-3, "r_em"=1e-3 , "Q_max"=3.4, "K_ads"=0.006,
           "K_des"= 0.001, "dd_beta" = 2, "Input_POM" = 0, "Input_DOM"=0, "CUE"=0.4)
units <- c("mgC mgC^-1 h^-1", "mgC / g soil", "mgC mgC^-1 h^-1", "mg C/g soil", "mgC mgC^-1 h^-1", "mg C/g soil", NA,
           NA, NA, NA, "mgC mgC^-1 h^-1", "mgC mgC^-1 h^-1", "mgC / g soil", "mgC mgC^-1 h^-1", "mgC mgC^-1 h^-1", NA,
           "mg C", "mg C", "mg C", "")

param_dt <- data.table("parameter" = params, "description" = desc, "units" = units, "value" = value)

times <- 0:36500

# JZ_mend <- memc_configure(params = param_dt,
#                            state = jz_initial_conditions,
#                            name = "MEND",
#                            F1 = "MM",
#                            F2 = "MM",
#                            F8 = "DD")
# out_mend <- memc_solve(mod = JZ_mend, time = times)



new_params <- memc_update_params(new_params = c("V_d"=1, "V_p"=5, "V_m"=1), param_table = param_dt)
JZ_com <- memc_configure(params = param_dt,
                           state = c(P=4.71, M=17.67, Q=0, D=0.148, B=0.8200, EP=0.0082, EM=0.0082, IC=0, Tot=23.484),
                           name = "COM",
                           F1 = "MM",
                           F2 = "MM",
                         F8 = "DD")
out_com <- memc_solve(mod = JZ_com, time = seq(0,36500, by=1))





# new_params <- memc_update_params(new_params = c("V_d"=0.5, "V_p"=0.001, "V_m"=0.001), param_table = param_dt)
# mod <- CORPSE_config
# mod$name <- "LIN"
# mod$params <- new_params
# mod$state <- c(4.7100, 17.6700, 0.0000,  0.520,  0.1480,  0.0520,  0.0520,  0.0000, 23.4840)
# names(mod$state) <- c("P", "M","Q","B","D","EP", "EM","IC","Tot")
# out_lin <- memc_solve(mod = mod, time = seq(0,36500, by=1))



new_params <- memc_update_params(new_params = c("V_d"=0.5, "V_p"=0.001, "V_m"=0.001), param_table = param_dt)
JZ_toy <- memc_configure(params = new_params,
                          state = c(P=4.71, M=17.67, Q=0, D=0.148, B=0.520, EP=0.052, EM=0.052, IC=0, Tot=23.484),
                          name = "TOY",
                          F1 = "RMM",
                          F2 = "MM")
out_toy <- memc_solve(mod = JZ_toy, time = seq(0,36500, by=1))



memc_results <- rbind(out_mend, out_com, out_lin, out_toy) %>%
  rename(memc = value)


write.csv(x = memc_results, file = here::here("legacy", "memc_results.csv"), row.names = FALSE)
