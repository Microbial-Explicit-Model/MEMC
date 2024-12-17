devtools::load_all()

MEND_model <- memc_configure(
  params = MEMC::default_params,
  state = MEMC::default_initial,
  name = "MEND",
  DOMuptake = "MM",
  POMdecomp = "MM",
  MBdecay = "LM"
)
usethis::use_data(MEND_model, overwrite = TRUE)

COMISSION_model <- memc_configure(
  params = MEMC::default_params,
  state = MEMC::default_initial,
  name = "COMISSION",
  DOMuptake = "MM",
  POMdecomp = "RMM",
  MBdecay = "LM"
)
usethis::use_data(COMISSION_model, overwrite = TRUE)

CORPSE_model <- memc_configure(
  params = MEMC::default_params,
  state = MEMC::default_initial,
  name = "CORPSE",
  DOMuptake = "RMM",
  POMdecomp = "LM",
  MBdecay = "LM"
)
usethis::use_data(CORPSE_model, overwrite = TRUE)


MEMS_model <- memc_configure(
  params = MEMC::default_params,
  state = MEMC::default_initial,
  name = "MEMS",
  DOMuptake = "LM",
  POMdecomp = "LM",
  MBdecay = "LM"
)
usethis::use_data(MEMS_model, overwrite = TRUE)

BAMS_model <- memc_configure(
  params = MEMC::default_params,
  state = MEMC::default_initial,
  name = "BAMS",
  DOMuptake = "MM",
  POMdecomp = "MM",
  MBdecay = "LM"
)
usethis::use_data(BAMS_model, overwrite = TRUE)


# Update the parameter data frame to use the density dependent MB decay.
param_df <- update_params(new_params = c("dd_beta" = 2),
                          param_table = MEMC::default_params)

MIMCS_model <- memc_configure(
  params = param_df,
  state = MEMC::default_initial,
  name = "MIMCS",
  DOMuptake = "MM",
  POMdecomp = "MM",
  MBdecay = "DD"
)
usethis::use_data(MIMCS_model, overwrite = TRUE)


model_configs <- rbind(MEND_model$table,
                       COMISSION_model$table,
                       CORPSE_model$table,
                       MEMS_model$table,
                       BAMS_model$table,
                       MIMCS_model$table)
usethis::use_data(model_configs, overwrite = TRUE)

