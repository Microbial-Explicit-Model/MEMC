# Taken from JZ's MEMC_Dec7[69].Rmd used to make sure that the MEMC package is
# returning the correct results.
library(FME)
library(deSolve)
library(data.table)

DIR <- here::here("tests", "testthat")
times=seq(from = 0, to = 100, by = 1)

# MEND configuration ------------------------------------------------------------------------------------------
solveMEND <- function(pars,times){
  derivs <- function(t, state, pars){
    with(as.list(c(state, pars)),{
      #default paramters
      # Carbon use effciency
      kD = 0.25
      kP = 50 #half saturation constant for POM
      kM = 250
      Kads = 0.006
      Kdes = 0.001
      Qmax = 3.4
      pEP=0.01
      pEM=0.01
      rEP=1e-3
      rEM=1e-3
      fD=0.5
      gD=0.5
      CUE=0.4
      Input=0 #input per day
      F1 <- Vd* MB *DOM /( kD + DOM)  ## microbial uptake
      F2 <- Vp* EP * POM / (kP + POM) # POM decomposition
      F3 <- Vm * EM * MOM / (kM + MOM) #MOAM decomposition
      F6 <- Kads * DOM *(1- QOM/ Qmax) ##adsorption
      F7 <- Kdes * QOM/Qmax  ##desorption
      F8 <- (1- pEP- pEM) * MB * 0.4*Vd* MB #microbial biomass decay (mr=0.4*vd)
      F9ep <- pEP * MB *0.4*Vd  #enzyme production (mr=0.4vd)
      F9em <- pEM * MB *0.4*Vd
      F10ep <- rEP * EP  #enzyme decay
      F10em <- rEM * EM

      dPOM <- (1 - gD) * F8- F2+Input
      dMOM <- (1 - fD) * F2 - F3+Input
      dMB <- F1*CUE - F8 - (F9em + F9ep) # Microbial growth= microbial uptake *CUE
      dDOM <- fD * F2 + gD * F8 + F3 + (F10em + F10ep)- F1 - (F6 - F7)
      dQOM <- F6 - F7
      dEP <- F9ep - F10ep
      dEM <- F9em - F10em
      dIC <- F1*(1-CUE)   #CO2 fllux
      dTot <- -F1*(1-CUE)+Input*2
      return(list(c(dPOM, dMOM, dQOM, dDOM, dMB,dEP,dEM, dIC, dTot)))
    })
  }

  state<-c(POM=4.71, MOM=17.67, QOM=0, DOM=0.148, MB=0.82, EP=0.0082, EM=0.0082, IC=0, Tot=23.484)#Ultisol

  return(ode(y=state, times=times, func=derivs, parms=pars))

}


output<-solveMEND(list (Vd=3,Vp=14, Vm=0.25), times)
result<-as.data.frame(output)

# Now format the results into a nice data frame instead of a wide  matrix.
out_jz <- data.table::melt(data.table::as.data.table(output),
                           measure.vars = c("POM",  "MOM",  "QOM",  "DOM",  "MB", "EP" , "EM", "IC", "Tot"),
                           variable.name = "variable",
                           value.name = 'jz_value')
out_jz$units <- 'mg C/g soil'
out_jz$variable <- gsub(out_jz$variable, pattern = "OM",  replacement = "")
out_jz$variable <- gsub(out_jz$variable, pattern = "MB",  replacement = "B")
out_jz$name <- "MEND"
out_jz_mend <- out_jz

# COMISSION configuration ------------------------------------------------------------------------------------------

solveCOM <- function(pars,times){
  derivs <- function(t, state, pars){
    with(as.list(c(state, pars)),{
      #default paramters
      # Carbon use effciency
      kD = 0.25
      kP = 50 #half saturation constant for POM
      kM = 250
      Kads = 0.006
      Kdes = 0.001
      Qmax = 3.4
      pEP=0.01
      pEM=0.01
      rEP=1e-3
      rEM=1e-3
      fD=0.5
      gD=0.5
      CUE=0.4
      Input=0 #input per day
      F1 <- Vd* MB *DOM /( kD + DOM)  ## microbial uptake
      F2 <- Vp* EP* POM / (kP + EP) # POM decomposition
      F3 <- Vm * EM * MOM / (kM + EM) #MOAM decomposition
      F6 <- Kads * DOM *(1- QOM/ Qmax) ##adsorption
      F7 <- Kdes * QOM/Qmax  ##desorption
      F8 <- (1- pEP- pEM)* MB *0.4*Vd *MB #microbial biomass decay (biomass decay rate=0.2*growth rate)
      F9ep <- pEP * MB *0.4*Vd  #enzyme production (maintaince respiration=0.1*total respiration)
      F9em <- pEM * MB *0.4*Vd
      F10ep <- rEP * EP  #enzyme decay
      F10em <- rEM * EM

      dPOM <- (1 - gD) * F8- F2 +Input
      dMOM <- (1 - fD) * F2 - F3+Input
      dMB <- F1*CUE - F8 - (F9em + F9ep) # Microbial growth= microbial uptake *CUE
      dDOM <- fD * F2 + gD * F8 + F3 + (F10em + F10ep)- F1 - (F6 - F7)
      dQOM <- F6 - F7
      dEP <- F9ep - F10ep
      dEM <- F9em - F10em
      dIC <- F1*(1-CUE)   #CO2 fllux
      dTot <- -F1*(1-CUE)+Input*2
      return(list(c(dPOM, dMOM, dQOM, dDOM, dMB,dEP,dEM, dIC, dTot)))
    })
  }


  #state<-c(POM=3.25, MOM=27.97, QOM=0, DOM=0.21, MB=0.64, EP=0.064, EM=0.064, IC=0, Tot=32.198)#Mollisol
  #state<-c(POM=4.25, MOM=11.04, QOM=0, DOM=0.17, MB=0.05, EP=0.005, EM=0.005, IC=0, Tot=15.52)#Gelisol
  state<-c(POM=4.71, MOM=17.67, QOM=0, DOM=0.148, MB=0.52, EP=0.052, EM=0.052, IC=0, Tot=23.484)#Ultisol
  #state<-c(POM=20.06, MOM=64.86, QOM=0, DOM=0.64, MB=0.86, EP=0.086, EM=0.086, IC=0, Tot=86.592)#Andisol

  #solved the mode
  #times <- seq(0, 365, by = 1)
  return(ode(y=state, times=times, func=derivs, parms=pars))

}

output<-solveCOM(list(Vd=1,Vp=5, Vm=1), times)


# Now format the results into a nice data frame instead of a wide  matrix.
out_jz <- data.table::melt(data.table::as.data.table(output),
                           measure.vars = c("POM",  "MOM",  "QOM",  "DOM",  "MB", "EP" , "EM", "IC", "Tot"),
                           variable.name = "variable",
                           value.name = 'jz_value')
out_jz$units <- 'mg C/g soil'
out_jz$variable <- gsub(out_jz$variable, pattern = "OM",  replacement = "")
out_jz$variable <- gsub(out_jz$variable, pattern = "MB",  replacement = "B")
out_jz$name <- "COMISSION"
out_jz_com <- out_jz

# CORPSE configuration ------------------------------------------------------------------------------------------
pars<- list (Vd=0.5,Vp=0.001, Vm=0.001)
solveLIN<- function(pars,times){
  derivs <- function(t, state, pars){
    with(as.list(c(state, pars)),{
      #default paramters
      kD = 0.25
      kP = 50 #half saturation constant for POM
      kM = 250
      Kads = 0.006
      Kdes = 0.001
      Qmax = 3.4
      pEP=0.01
      pEM=0.01
      rEP=1e-3
      rEM=1e-3
      fD=0.5
      gD=0.5
      CUE=0.4
      Input=0 #input per day
      F1 <- Vd* MB *DOM /( kD + MB)  ## microbial uptake
      F2 <- Vp* POM # POM decomposition
      F3 <- Vm  * EM * MOM / (kM + MOM) #MOAM decomposition
      F6 <- Kads * DOM *(1- QOM/ Qmax) ##adsorption
      F7 <- Kdes * QOM/Qmax  ##desorption
      F8 <- (1- pEP- pEM)* MB *0.4*Vd *MB #microbial biomass decay (biomass decay rate=0.2*growth rate)
      F9ep <- pEP * MB *0.4*Vd  #enzyme production (maintaince respiration=0.1*total respiration)
      F9em <- pEM * MB *0.4*Vd
      F10ep <- rEP * EP  #enzyme decay
      F10em <- rEM * EM

      dPOM <- (1 - gD) * F8- F2+Input
      dMOM <- (1 - fD) * F2 - F3+Input
      dMB <- F1*CUE - F8 - (F9em + F9ep) # Microbial growth= microbial uptake *CUE
      dDOM <- fD * F2 + gD * F8 + F3 + (F10em + F10ep)- F1 - (F6 - F7)+Input
      dQOM <- F6 - F7
      dEP <- F9ep - F10ep
      dEM <- F9em - F10em
      dIC <- F1*(1-CUE)   #CO2 fllux
      dTot <- -F1*(1-CUE)+Input*3
      return(list(c(dPOM, dMOM, dQOM, dDOM, dMB,dEP,dEM, dIC, dTot)))
    })
  }


  #state<-c(POM=3.25, MOM=27.97, QOM=0, DOM=0.21, MB=0.64, EP=0.064, EM=0.064, IC=0, Tot=32.198)#Mollisol
  #state<-c(POM=4.25, MOM=11.04, QOM=0, DOM=0.17, MB=0.05, EP=0.005, EM=0.005, IC=0, Tot=15.52)#Gelisol
  state<-c(POM=4.71, MOM=17.67, QOM=0, DOM=0.148, MB=0.52, EP=0.052, EM=0.052, IC=0, Tot=23.484)#Ultisol
  #state<-c(POM=20.06, MOM=64.86, QOM=0, DOM=0.64, MB=0.86, EP=0.086, EM=0.086, IC=0, Tot=86.592)#Andisol

  #solved the mode
  #times <- seq(0, 365, by = 1)
  ox<-ode(y=state, times=times, func=derivs, parms=pars)

  data.frame(ox)
}

output<-solveLIN(pars, times)

# Now format the results into a nice data frame instead of a wide  matrix.
out_jz <- data.table::melt(data.table::as.data.table(output),
                           measure.vars = c("POM",  "MOM",  "QOM",  "DOM",  "MB", "EP" , "EM", "IC", "Tot"),
                           variable.name = "variable",
                           value.name = 'jz_value')
out_jz$units <- 'mg C/g soil'
out_jz$variable <- gsub(out_jz$variable, pattern = "OM",  replacement = "")
out_jz$variable <- gsub(out_jz$variable, pattern = "MB",  replacement = "B")
out_jz$name <- "CORPSE"
out_jz_lin <- out_jz

# ------------------------------------------------------------------------------------------------------------
out <- rbind(out_jz_mend, out_jz_com, out_jz_lin)
write.csv(out, file = file.path(DIR, "jz_output.csv"), row.names = FALSE)
