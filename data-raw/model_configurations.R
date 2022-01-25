# Typically this script should be run with the current developmental version of the branch.
devtools::load_all()
# Otherwise will need to load the MEMC package
# library(MEMC)

# The basic MEND 2013 model
MEND_model <- configure_model(params = default_params, state = default_initial,
                              carbon_pools_func = carbon_pools,
                              carbon_fluxes_func = carbon_fluxes,
                              name = "MEND")
usethis::use_data(MEND_model, overwrite = TRUE, internal = FALSE)


# Commission model configuration.
COMISSION_model <- configure_model(params = default_params,
                                   state = default_initial,
                                   carbon_pools_func = carbon_pools,
                                   carbon_fluxes_func = carbon_fluxes,
                                   name = "COMISSION",
                                   POMdecomp = "RMM")
usethis::use_data(COMISSION_model, overwrite = TRUE, internal = FALSE)


