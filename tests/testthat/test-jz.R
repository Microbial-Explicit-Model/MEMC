# # Compare out package outputs with the results from JZ, this is a proof of concept that the package
# # is working as expected.
#
# library(data.table)
#
#
# tol <- 1e-4
#
# jz_initial_conditions <- c(POM = 4.71, MOM = 17.67, QOM = 0, MB = 0.82,
#                            DOM = 0.148, EP = 0.0082, EM = 0.0082, IC = 0,
#                            Tot = 23.484)
#
# params <- c("V_p", "K_p", "V_m", "K_m", "V_d", "K_d", "f_d", "g_d", "p_ep",
#             "p_em", "r_ep", "r_em", "Q_max", "K_ads",
#             "K_des", "dd_beta", "Input_POM", "Input_DOM", "CUE")
# desc <- c("maximum specific decomposition rate for P by EP", "half-saturation constant for decomposition of P",
#           "maximum specific decomposition rate for M by EM",
#           "half-saturation constant for decomposition of M",
#           "maximum specific uptake rate of D for growth of B",
#           "half-saturation constant of uptake of D for growth of B",
#           "fraction of decomposed P allocated to D", "fraction of dead B allocated to D",
#           "fraction of mR for production of EP", "fraction of mR for production of EM",
#           "turnover rate of EP", "turnover rate of EM", "maximum DOC sorption capacity",
#           "specific adsorption rate", "desorption rate", "strength of density dependent microbial decay",
#           "POM input", "DOM input", "M input", "carbon use efficiency")
# value <- c("V_p" = 14, "K_p" = 50, "V_m" = 0.25, "K_m" = 250, "V_d" = 3, "K_d"= 0.25,
#            "f_d" = 0.5, "g_d" = 0.5, "p_ep" = 0.01, "p_em" = 0.01, "r_ep" = 1e-3,
#            "r_em" = 1e-3 , "Q_max" = 3.4, "K_ads" = 0.006, "K_des"= 0.001, "dd_beta" = 2,
#            "Input_POM" = 0, "Input_DOM" = 0, "CUE" = 0.4)
# units <- c("mgC mgC^-1 h^-1", "mgC / g soil", "mgC mgC^-1 h^-1", "mg C/g soil",
#            "mgC mgC^-1 h^-1", "mg C/g soil", NA,
#            NA, NA, NA, "mgC mgC^-1 h^-1", "mgC mgC^-1 h^-1", "mgC / g soil",
#            "mgC mgC^-1 h^-1", "mgC mgC^-1 h^-1", NA,
#            "mg C", "mg C", "mg C", "")
#
# param_dt <- data.table("parameter" = params, "description" = desc, "units" = units, "value" = value)
# times <- 0:36500
#
# # Helper function that calculates the difference between two data frame
# # Args
# #   jz: dataframe of the legacy data from jz's original implementation of the model
# #   memc: dataframe for the model results produced by the package
# # Return: dataframe containning, the new, comparison, and absolute difference between the model results
# compare_results <- function(jz, memc){
#   cond <- all(c("time", "variable", "jz_value") %in% names(jz))
#   assertthat::assert_that(cond)
#
#   cond <- all(c("time", "variable", "value") %in% names(memc))
#   assertthat::assert_that(cond)
#
#
#   joined_df <- merge(jz, memc, all=FALSE)
#   assertthat::assert_that(nrow(joined_df) == nrow(jz))
#
#   joined_df$diff <- abs(joined_df$jz_value - joined_df$value)
#   return(joined_df)
# }
#
#
#
# test_that("MEND", {
#
#   # Read in the comparison data
#   jz_results <- data.table(read.csv("jz_comps/jz_mend.csv"))
#   names(jz_results) <- c("time", "variable", "jz_value", "units", "model")
#
#   # Set up the mend model and run results
#   mod <- configure_model(params = param_dt,
#                          state = jz_initial_conditions,
#                          name = "JZ mend",
#                          DOMdecomp = "MM",
#                          POMdecomp = "MM",
#                          MBdecay = "DD")
#   out_JZ_mend <- memc_solve(mod = mod, time = times)
#
#   diff_df <- compare_results(jz = jz_results, memc = out_JZ_mend)
#   expect_true(all(diff_df$diff <= tol))
#
#   # # Diagnostic plots to use when not passing the test
#   # library(dplyr)
#   # library(ggplot2)
#   # ggplot(data = diff_df %>%
#   #          filter(time <= 100)) +
#   #   geom_line(aes(x = time, y = jz_value, color = "original imp."), size = 1) +
#   #   geom_line(aes(x = time, y = value, color = "memc imp."),
#   #             linetype = 2, size = 1) +
#   #   facet_wrap("variable", scales = "free")
#   #
#   # ggplot(data = diff_df) +
#   #   geom_line(aes(x = time, y = diff)) +
#   #   facet_wrap("variable", scales = "free") +
#   #   ylim(-0.05, 0.05)
#
#
# })
#
#
# test_that("COM", {
#
#   # Read in the comparison data
#   jz_results <- data.table(read.csv("jz_comps/jz_com.csv"))
#   names(jz_results) <- c("time", "variable", "jz_value", "units", "model")
#
#   state<-c(POM=4.71, MOM=17.67, QOM=0,  MB=0.52, DOM=0.148, EP=0.052, EM=0.052, IC=0, Tot=23.484) #Ultisol
#
#   # Most of parameters were defined the same way, update the ones that are different.
#   params_to_use <- update_params(new_params = c("V_d" = 1, "V_p" = 5, "V_m" = 1), param_table = param_dt)
#
#   # Set up the mend model and run results
#   mod <- configure_model(params = params_to_use,
#                          state = state,
#                          name = "JZ com",
#                          DOMdecomp = "MM",
#                          POMdecomp = "RMM",
#                          MBdecay = "DD")
#   out_JZ_com <- memc_solve(mod = mod, time = times)
#
#   diff_df <- compare_results(jz = jz_results, memc = out_JZ_com)
#   expect_true(all(diff_df$diff <= tol))
#
#   # # Diagnostic plots to use when not passing the test
#   # library(dplyr)
#   # library(ggplot2)
#   # ggplot(data = diff_df %>% filter(time <= 500)
#   #        ) +
#   #   geom_line(aes(x = time, y = jz_value, color = "original imp."), size = 1) +
#   #   geom_line(aes(x = time, y = value, color = "memc imp."),
#   #             linetype = 2, size = 1) +
#   #   facet_wrap("variable", scales = "free")
#   #
#   # ggplot(data = diff_df %>% filter(time <= 100) #%>%  filter(!variable %in% c("IC", "MOM", "Tot"))
#   #        ) +
#   #   geom_line(aes(x = time, y = diff, color = variable)) +
#   #  # facet_wrap("variable", scales = "free") +
#   #   #ylim(-0.05, 0.05) +
#   #   NULL
#
# })
#
#
# test_that("LIN", {
#
#   # Most of parameters were defined the same way, update the ones that are different.
#   state<-c(POM=4.71, MOM=17.67, QOM=0, MB=0.52,  DOM=0.148, EP=0.052, EM=0.052, IC=0, Tot=23.484)
#   params_to_use <- update_params(new_params = c("V_d" = 0.5, "V_p" = 0.001, "V_m" = 0.001),
#                                  param_table = param_dt)
#
#   # Read in the comparison data
#   jz_results <- data.table(read.csv("jz_comps/jz_lin.csv"))
#   names(jz_results) <- c("time", "variable", "jz_value", "units", "model")
#
#   # Set up the mend model and run results
#   mod <- configure_model(params = params_to_use,
#                          state = state,
#                          name = "JZ LIN",
#                          DOMdecomp = "RMM",
#                          POMdecomp = "LM",
#                          MBdecay = "DD")
#   out_JZ_lin <- memc_solve(mod = mod, time = unique(jz_results$time))
#   diff_df <- compare_results(jz = jz_results, memc = out_JZ_lin)
#   expect_true(all(diff_df$diff <= tol))
#
#     # # Diagnostic plots to use when not passing the test
#     # library(dplyr)
#     # library(ggplot2)
#     # ggplot(data = diff_df  %>% filter(time <= 10)
#     #        ) +
#     #   geom_line(aes(x = time, y = jz_value, color = "original imp."), size = 1) +
#     #   geom_line(aes(x = time, y = value, color = "memc imp."),
#     #             linetype = 2, size = 1) +
#     #   facet_wrap("variable", scales = "free")
#     #
#     # ggplot(data = diff_df) +
#     #   geom_line(aes(x = time, y = diff)) +
#     #   facet_wrap("variable", scales = "free") +
#     #   ylim(-0.05, 0.05) +
#     #   xlim(0, 10) +
#     #   NULL
#
#
# })
#
# test_that("TOY", {
#
#   # Most of parameters were defined the same way, update the ones that are different.
#   state<-c(POM=4.71, MOM=17.67, QOM=0,  MB=0.52, DOM=0.148, EP=0.052, EM=0.052, IC=0, Tot=23.484) #Ultisol
#   params_to_use <- update_params(new_params = c("V_d" = 0.5, "V_p" = 0.001, "V_m" = 0.001),
#                                  param_table = param_dt)
#
#   # Read in the comparison data
#   jz_results <- data.table(read.csv("jz_comps/jz_toy.csv"))
#   names(jz_results) <- c("time", "variable", "jz_value", "units", "model")
#
#   # Set up the mend model and run results
#   mod <- configure_model(params = params_to_use,
#                          state = state,
#                          name = "JZ TOY",
#                          DOMdecomp = "RMM",
#                          POMdecomp = "MM",
#                          MBdecay = "DD")
#   out_JZ_toy <- memc_solve(mod = mod, time = unique(jz_results$time))
#
#   diff_df <- compare_results(jz = jz_results, memc = out_JZ_toy)
#   expect_true(all(diff_df$diff <= tol))
#
#   # # Diagnostic plots to use when not passing the test
#   # library(dplyr)
#   # library(ggplot2)
#   # ggplot(data = diff_df %>% filter(time <= 500)
#   # ) +
#   #   geom_line(aes(x = time, y = jz_value, color = "original imp."), size = 1) +
#   #   geom_line(aes(x = time, y = value, color = "memc imp."),
#   #             linetype = 2, size = 1) +
#   #   facet_wrap("variable", scales = "free")
#   #
#   # ggplot(data = diff_df %>% filter(time <= 5)
#   # ) +
#   #   geom_line(aes(x = time, y = diff)) +
#   #   facet_wrap("variable", scales = "free") +
#   #   #ylim(-0.05, 0.05) +
#   #   NULL
#
# })
#
#
#
#
#
#
#
