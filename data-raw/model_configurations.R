# Typically this script should be run with the current developmental version of the branch.
devtools::load_all()
# Otherwise will need to load the MEMC package
# library(MEMC)

# The basic MEND 2013 model
MEND_model <- configure_model(params = default_params, state = default_initial,
                              carbon_pools_func = carbon_pools,
                              carbon_fluxes_func = carbon_fluxes,
                              name = "MEND",
                              DOMdecomp = "MM",
                              POMdecomp = "MM",
                              MBdecay = "LM")
usethis::use_data(MEND_model, overwrite = TRUE, internal = FALSE)


# Commission model configuration.
COMISSION_model <- configure_model(params = default_params,
                                   state = default_initial,
                                   carbon_pools_func = carbon_pools,
                                   carbon_fluxes_func = carbon_fluxes,
                                   name = "COMISSION",
                                   DOMdecomp = "RMM",
                                   POMdecomp = "MM",
                                   MBdecay = "LM")
usethis::use_data(COMISSION_model, overwrite = TRUE, internal = FALSE)


# Corpse model configuration.
CORPSE_model <- configure_model(params = default_params,
                                state = default_initial,
                                carbon_pools_func = carbon_pools,
                                carbon_fluxes_func = carbon_fluxes,
                                name = "CORPSE",
                                DOMdecomp = "RMM",
                                POMdecomp = "LM",
                                MBdecay = "LM")
usethis::use_data(CORPSE_model, overwrite = TRUE, internal = FALSE)


