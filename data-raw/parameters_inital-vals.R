# Generate the interal package data, the parameters and the default intial states.

DIR <- here::here("data-raw")

# MEND 2013 -----------------------------------------------------------------------------------------
# Wang, G., Post, W.M. and Mayes, M.A. (2013), Development of microbial‐enzyme‐mediated decomposition
# model parameters through steady‐state and dynamic analyses. Ecological Applications,
# 23: 255-272. doi:10.1890/12-0681.1

MEND_params <- read.csv(file.path(DIR, "MEND_params.csv"), stringsAsFactors = FALSE)
usethis::use_data(MEND_params, overwrite = TRUE, internal = FALSE)


# Inital state values
# Define the inital state values, these values are taken directly from the manuscript.
B = 2
D = 1
P = 10
Q = 0.1
M = 5
EP = 0.00001
EM = 0.00001
IC = 0
Tot = 18.10002

MEND_initalState <- c(P = P,  M = M,  Q = Q,  B = B,  D = D,  EP = EP,  EM = EM,  IC = IC,  Tot = Tot)
usethis::use_data(MEND_initalState, overwrite = TRUE, internal = FALSE)

# 2 Pool MEND  -----------------------------------------------------------------------------------------
# Citation?
MEND2_params <- read.csv(file.path(DIR, "MEND2_params.csv"), stringsAsFactors = FALSE)
usethis::use_data(MEND2_params, overwrite = TRUE, internal = FALSE)


B = 0.5
D1 = 0.1
D2 = 0.4
P1 = 4
P2 = 2
Q1 = 0.1
Q2 = 0.9
M = 10
EP1 = 0.00001
EP2 = 0.00001
EM = 0.00001
IC =  0
Tot = B+D1+D2+P1+P2+M+Q1+Q2+EP1+EP2+EM

MEND2_initalState <- c(P1 = P1, P2 = P2, M = M, B = B, D1 = D1, D2 = D2, Q1 = Q1,
                       Q2 = Q2, EP1 = EP1, EP2 = EP2, EM = EM, IC = IC, Tot = Tot)
usethis::use_data(MEND2_initalState, overwrite = TRUE, internal = FALSE)




