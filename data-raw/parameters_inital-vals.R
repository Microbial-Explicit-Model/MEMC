# Generate the internal package data, the parameters and the default initial states.

DIR <- here::here("data-raw")

# The default parameter values are based off of MEND 2013.
# Wang, G., Post, W.M. and Mayes, M.A. (2013), Development of microbial‐enzyme‐mediated decomposition
# model parameters through steady‐state and dynamic analyses. Ecological Applications,
# 23: 255-272. doi:10.1890/12-0681.1

memc_params <-
  read.csv(file.path(DIR, "memc_params.csv"), stringsAsFactors = FALSE)
usethis::use_data(memc_params, overwrite = TRUE, internal = FALSE)


# Initial state values also come from  MEND 2013.
# Wang, G., Post, W.M. and Mayes, M.A. (2013), Development of microbial‐enzyme‐mediated decomposition
# model parameters through steady‐state and dynamic analyses. Ecological Applications,
# 23: 255-272. doi:10.1890/12-0681.1
MB = 2
DOC = 1
POC = 10
QOC = 0.1
MOC = 5
EP = 0.00001
EM = 0.00001
IC = 0
Tot = 18.10002

memc_initial_state <-
  c(
    POC = POC,
    MOC = MOC,
    QOC = QOC,
    MB = MB,
    DOC = DOC,
    EP = EP,
    EM = EM,
    IC = IC,
    Tot = Tot
  )
usethis::use_data(memc_initial_state, overwrite = TRUE, internal = FALSE)
