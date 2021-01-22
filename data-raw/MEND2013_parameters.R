# Wang, G., Post, W.M. and Mayes, M.A. (2013), Development of microbial‐enzyme‐mediated decomposition
# model parameters through steady‐state and dynamic analyses. Ecological Applications,
# 23: 255-272. doi:10.1890/12-0681.1

# Parameter table -----------------------------------------------------------------------------------------
# Model parameter/inputs names from table 2.
params <- c('V.p',
           'K.p',
           'V.m',
           'K.m',
           'V.d',
           'K.d',
           'm.r',
           'E.c',
           'f.d',
           'g.d',
           'p.ep',
           'p.em',
           'r.ep',
           'r.em',
           'Q.max',
           'K.ads',
           'K.des',
           'K.ba',
           'I.p',
           'I.d',
           'fI.d')

# Model parameter/inputs description from table 2.
description <- c('maximum specific decomposition rate for P by EP',
                 'half-saturation constant for decomposition of P',
                 'maximum specific decomposition rate for M by EM',
                 'half-saturation constant for decomposition of M',
                 'maximum specific uptake rate of D for growth of B',
                 'half-saturation constant of uptake of D for growth of B',
                 'specific maintenance factor or rate',
                 'carbon use efficiency',
                 'fraction of decomposed P allocated to D',
                 'fraction of dead B allocated to D',
                 'fraction of mR for production of EP',
                 'fraction of mR for production of EM',
                 'turnover rate of EP',
                 'turnover rate of EM',
                 'maximum DOC sorption capacity',
                 'specific adsorption rate',
                 'desorption rate',
                 'binding affinity',
                 'input rate of P',
                 'input rate of D',
                 'ratio of ID to IP')

# Model parameter/inputs units from table 2.
units <- c('mgC mgC^-1 h^-1',
           'mgC / g soil',
           'mgC mgC^-1 h^-1',
           'mg C/g soil',
           'mgC mgC^-1 h^-1',
           'mg C/g soil',
           'mgC mgC^-1 h^-1',
           rep(NA, 5),
           'mgC mgC^-1 h^-1',
           'mgC mgC^-1 h^-1',
           'mgC / g soil',
           'mgC mgC^-1 h^-1',
           'mgC mgC^-1 h^-1',
           '(mgC/soil)^-1',
           'mgC mgC^-1 h^-1',
           'mgC mgC^-1 h^-1',
           NA)

# Create the data table of the parameters.
MEND2013_params <- data.table::data.table(parameter = params,
                                                  description = description,
                                                  units = units, value = NA_real_)

# Model parameter/inputs values from table 2.
MEND2013_params[parameter == 'V.p', ]$value <- 2.5
MEND2013_params[parameter == 'm.r', ]$value <- 2.8e-4
MEND2013_params[parameter == 'E.c', ]$value <- 0.47
MEND2013_params[parameter == 'f.d', ]$value <- 0.5
MEND2013_params[parameter == 'g.d', ]$value <- 0.5
MEND2013_params[parameter == 'r.ep', ]$value <- 1e-3
MEND2013_params[parameter == 'r.em', ]$value <- 1e-3
MEND2013_params[parameter == 'p.ep', ]$value <- 1e-2
MEND2013_params[parameter == 'p.em', ]$value <- 1e-2
MEND2013_params[parameter == 'Q.max', ]$value <- 1.7
MEND2013_params[parameter == 'K.ba', ]$value <- 6
MEND2013_params[parameter == 'K.des', ]$value <- 1e-3
MEND2013_params[parameter == 'K.ads', ]$value <- MEND2013_params[parameter == 'K.ba', ]$value * MEND2013_params[parameter == 'K.des', ]$value
MEND2013_params[parameter == 'K.d', ]$value <- 0.26
MEND2013_params[parameter == 'V.d', ]$value <- 5e-4
MEND2013_params[parameter == 'K.p', ]$value <- 50
MEND2013_params[parameter == 'V.m', ]$value <- 1
MEND2013_params[parameter == 'K.m', ]$value <- 250
MEND2013_params[parameter == 'K.m', ]$value <- 250
MEND2013_params[parameter == 'I.p', ]$value <- 8e-05
MEND2013_params[parameter == 'I.d', ]$value <- 8e-05
MEND2013_params[parameter == 'fI.d', ]$value <- MEND2013_params[parameter == 'I.d', ]$value /  MEND2013_params[parameter == 'I.p', ]$value

assertthat::assert_that(sum(is.na(MEND2013_params$value)) == 0, msg = 'Not all default parameter values have been defined.')

usethis::use_data(x = MEND2013_params, overwrite = TRUE)

# Inital state values -----------------------------------------------------------------------------------
# Define the inital state values, these values are taken directly from the manuscript.
B = 2; D = 1
P = 10; Q = 0.1
M = 5; EP = 0.00001
EM =  0.00001; IC = 0
Tot = 18.10002

state <- c(P = P,  M = M,  Q = Q,  B = B,  D = D,  EP = EP,  EM = EM,  IC = IC,  Tot = Tot)
MEND2013_initalState <- state
usethis::use_data(x = MEND2013_initalState, overwrite = TRUE)



