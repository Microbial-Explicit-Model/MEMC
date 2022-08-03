devtools::load_all()

MEND_model <- configure_model(
  params = MEMC::default_params,
  state = MEMC::default_initial,
  name = "MEND",
  DOMdecomp = "MM",
  POMdecomp = "MM",
  MBdecay = "DD"
)
usethis::use_data(MEND_model, overwrite = TRUE)

COMISSION_model <- configure_model(
  params = MEMC::default_params,
  state = MEMC::default_initial,
  name = "COMISSION",
  DOMdecomp = "MM",
  POMdecomp = "RMM",
  MBdecay = "DD"
)
usethis::use_data(COMISSION_model, overwrite = TRUE)


CORPSE_model <- configure_model(
  params = MEMC::default_params,
  state = MEMC::default_initial,
  name = "CORPSE",
  DOMdecomp = "RMM",
  POMdecomp = "LM",
  MBdecay = "DD"
)
usethis::use_data(CORPSE_model, overwrite = TRUE)
