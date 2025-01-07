devtools::load_all()

MEND_model <- memc_configure(
  params = memc_params,
  state = MEMC::default_initial,
  name = "MEND",
  DOMuptake = "MM",
  POMdecomp = "MM",
  MBdecay = "LM"
)
usethis::use_data(MEND_model, overwrite = TRUE)

COMISSION_model <- memc_configure(
  params = memc_params,
  state = MEMC::default_initial,
  name = "COMISSION",
  DOMuptake = "MM",
  POMdecomp = "RMM",
  MBdecay = "LM"
)
usethis::use_data(COMISSION_model, overwrite = TRUE)

CORPSE_model <- memc_configure(
  params = memc_params,
  state = MEMC::default_initial,
  name = "CORPSE",
  DOMuptake = "RMM",
  POMdecomp = "LM",
  MBdecay = "LM"
)
usethis::use_data(CORPSE_model, overwrite = TRUE)


MEMS_model <- memc_configure(
  params = memc_params,
  state = MEMC::default_initial,
  name = "MEMS",
  DOMuptake = "LM",
  POMdecomp = "LM",
  MBdecay = "LM"
)
usethis::use_data(MEMS_model, overwrite = TRUE)

BAMS_model <- memc_configure(
  params = memc_params,
  state = MEMC::default_initial,
  name = "BAMS",
  DOMuptake = "MM",
  POMdecomp = "MM",
  MBdecay = "LM"
)
usethis::use_data(BAMS_model, overwrite = TRUE)


# Update the parameter data frame to use the density dependent MB decay.
param_df <- memc_update_params(new_params = c("dd_beta" = 2),
                          param_table = memc_params)

MIMCS_model <- memc_configure(
  params = param_df,
  state = MEMC::default_initial,
  name = "MIMCS",
  DOMuptake = "MM",
  POMdecomp = "MM",
  MBdecay = "DD"
)
usethis::use_data(MIMCS_model, overwrite = TRUE)

# A list of all available models in MEMC
memc_all_models <- list(MEND = MEND_model,
                        COMISSION = COMISSION_model,
                        CORPSE = CORPSE_model,
                        MEMS = MEMS_model,
                        BAMS = BAMS_model,
                        MIMICS = MIMCS_model)
usethis::use_data(memc_all_models, overwrite = TRUE)

# A data table, built from memc_all_models, summarizing model configurations
model_configs <- lapply(memc_all_models,
                        function(x) x$table)
model_configs <- do.call(rbind, model_configs)
rownames(model_configs) <- NULL
usethis::use_data(model_configs, overwrite = TRUE)
