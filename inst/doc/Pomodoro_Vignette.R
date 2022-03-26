## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pomodoro)

## ----RF, echo=FALSE-----------------------------------------------------------
sample_data <- sample_data[c(1:750),]
yvar  <- c("Loan.Type")
xvar <- c("sex", "married", "age", "havejob", "educ", "political.afl",
"rural", "region", "fin.intermdiaries", "fin.knowldge", "income")
BchMk.RF <- RF_Model(sample_data, c(xvar, "networth"), yvar )
BchMk.RF$Roc$auc

## ----EstModel, echo=T---------------------------------------------------------
sample_data <- sample_data[c(1:750),]
yvar <- c("Loan.Type")
xvar <- c("sex", "married", "age", "havejob", "educ", "political.afl",
"rural", "region", "fin.intermdiaries", "fin.knowldge", "income")
CCP.RF <- Estimate_Models(sample_data, yvar, xvec = xvar, exog = "political.afl",
xadd = c("networth", "networth_homequity", "liquid.assets"),
type = "RF", dnames = c("0","1"))

## ----ComPerf, echo=T----------------------------------------------------------
Sub.CCP.RF <- list(Mdl.1 = CCP.RF$EstMdl$`D.1+networth`,
Mdl.0 = CCP.RF$EstMdl$`D.0+networth`)
CCP.NoCCP.RF <- Combined_Performance (Sub.CCP.RF)

