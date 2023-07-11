devtools::load_all()

MEND_model <- configure_model(
  params = MEMC::default_params,
  state = MEMC::default_initial,
  name = "MEND",
  DOMdecomp = "MM",
  POMdecomp = "MM",
  MBdecay = "LM"
)
usethis::use_data(MEND_model, overwrite = TRUE)

COMISSION_model <- configure_model(
  params = MEMC::default_params,
  state = MEMC::default_initial,
  name = "COMISSION",
  DOMdecomp = "RMM",
  POMdecomp = "MM",
  MBdecay = "LM"
)
usethis::use_data(COMISSION_model, overwrite = TRUE)

# Update the parameter data frame to use the density dependent MB decay.

param_df <- update_params(new_params = c("dd_beta" = 2),
                          param_table = MEMC::default_params)

CORPSE_model <- configure_model(
  params = param_df,
  state = MEMC::default_initial,
  name = "CORPSE",
  DOMdecomp = "LM",
  POMdecomp = "RMM",
  MBdecay = "DD"
)
usethis::use_data(CORPSE_model, overwrite = TRUE)

MIMCS_model <- configure_model(
  params = param_df,
  state = MEMC::default_initial,
  name = "MIMCS",
  DOMdecomp = "MM",
  POMdecomp = "MM",
  MBdecay = "DD"
)
usethis::use_data(MIMCS_model, overwrite = TRUE)

MEMS_model <- configure_model(
  params = param_df,
  state = MEMC::default_initial,
  name = "MEMS",
  DOMdecomp = "ECA",
  POMdecomp = "ECA",
  MBdecay = "DD"
)
usethis::use_data(MIMCS_model, overwrite = TRUE)






