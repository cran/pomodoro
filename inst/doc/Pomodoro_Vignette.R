## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pomodoro)

## ----MLR, echo=FALSE----------------------------------------------------------
sample_data <- sample_data[1:500,]
yvar  <- c("Loan.Type")
m2.xvar0 <- c("sex", "married", "age", "havejob", "educ", "rural", "region","income","political.afl")
#m2.xvar1 <- c(m2.xvar0, "networth")   # with "networth"
BchMk.MLM.1 <- RF_Model(sample_data, c(m2.xvar0,"networth"), yvar)
#BchMk.MLR.1
BchMk.MLM.1$finalModel
BchMk.MLM.1$Roc$auc

## ----EstModel, echo=FALSE-----------------------------------------------------
library(dplyr)
CCP <- sample_data %>% filter(sample_data$political.afl==1)
NoCCP <- sample_data %>% filter(sample_data$political.afl==0)

#yvar  <- c("Loan.Type")
#m2.xvar0 <- c("sex", "married", "age", "havejob", "educ", "rural", "region","income","political.afl") #, "ethnicity" "class.of.HH",


# CCP.MLR.1 <-  RF_Model(CCP, m2.xvar1, yvar)
# NoCCP.MLR.1 <-  RF_Model(NoCCP, m2.xvar1, yvar)

CCP.RF.1  <- Estimate_Models(sample_data, yvar = c("Loan.Type"), exog = "political.afl", xvec = m2.xvar0, xadd="networth", type="RF", dnames=c("0","1"))
CCP.RF.1

## ----ComPerf, echo=FALSE------------------------------------------------------
library(dplyr)
sample_data <- sample_data[c(1:750),]
m2.xvar0 <- c("sex", "married", "age", "havejob", "educ", "rural", "region","income") #, "ethnicity" "class.of.HH",
m2.xvar1 <- c(m2.xvar0, "networth")   # with "networth"
CCP <- sample_data %>% filter(sample_data$political.afl==1)
NoCCP <- sample_data %>% filter(sample_data$political.afl==0)
CCP.RF <-  RF_Model(CCP, m2.xvar1, yvar)
NoCCP.RF <-  RF_Model(NoCCP, m2.xvar1, yvar)
Sub.CCP.RF <- list(Mdl.1 = CCP.RF,  Mdl.2= NoCCP.RF)
CCP.NoCCP.RF <- Combined_Performance(Sub.CCP.RF)
CCP.NoCCP.RF$Roc$auc

