devtools::load_all()

MEND_config <- memc_configure(
  params = memc_params,
  state = memc_initial_state,
  name = "MEND",
  DOMuptake = "MM",
  POMdecomp = "MM",
  MBdecay = "LM"
)
usethis::use_data(MEND_config, overwrite = TRUE)

COMISSION_config <- memc_configure(
  params = memc_params,
  state = memc_initial_state,
  name = "COMISSION",
  DOMuptake = "MM",
  POMdecomp = "RMM",
  MBdecay = "LM"
)
usethis::use_data(COMISSION_config, overwrite = TRUE)

CORPSE_config <- memc_configure(
  params = memc_params,
  state = memc_initial_state,
  name = "CORPSE",
  DOMuptake = "RMM",
  POMdecomp = "LM",
  MBdecay = "LM"
)
usethis::use_data(CORPSE_config, overwrite = TRUE)


MEMS_config <- memc_configure(
  params = memc_params,
  state = memc_initial_state,
  name = "MEMS",
  DOMuptake = "LM",
  POMdecomp = "LM",
  MBdecay = "LM"
)
usethis::use_data(MEMS_config, overwrite = TRUE)

BAMS_config <- memc_configure(
  params = memc_params,
  state = memc_initial_state,
  name = "BAMS",
  DOMuptake = "MM",
  POMdecomp = "MM",
  MBdecay = "LM"
)
usethis::use_data(BAMS_config, overwrite = TRUE)


# Update the parameter data frame to use the density dependent MB decay.
param_df <- memc_update_params(new_params = c("dd_beta" = 2),
                          param_table = memc_params)

MIMCS_config <- memc_configure(
  params = param_df,
  state = memc_initial_state,
  name = "MIMCS",
  DOMuptake = "MM",
  POMdecomp = "MM",
  MBdecay = "DD"
)
usethis::use_data(MIMCS_config, overwrite = TRUE)

# A list of all available models in MEMC
memc_all_configs <- list(MEND = MEND_config,
                        COMISSION = COMISSION_config,
                        CORPSE = CORPSE_config,
                        MEMS = MEMS_config,
                        BAMS = BAMS_config,
                        MIMICS = MIMCS_config)
class(memc_all_configs) <- c("memc_all_configs", class(memc_all_configs))
usethis::use_data(memc_all_configs, overwrite = TRUE)
