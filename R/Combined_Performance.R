#' Combined Performance of the Data Splits
#'
#'
#' @param Sub.Est.Mdls is the total perfomance of exog.
#' @return The output from  \code{\link{Combined_Performance}}.
#' @export
#' @examples
#' \donttest{
#' sample_data <- sample_data[c(1:750),]
#' yvar <- c("Loan.Type")
#' xvar <- c("sex", "married", "age", "havejob", "educ", "political.afl",
#' "rural", "region", "fin.intermdiaries", "fin.knowldge", "income")
#' CCP.RF <- Estimate_Models(sample_data, yvar, xvec = xvar, exog = "political.afl",
#' xadd = c("networth", "networth_homequity", "liquid.assets"),
#' type = "RF", dnames = c("0","1"))
#' Sub.CCP.RF <- list (Mdl.1 = CCP.RF$EstMdl$`D.1+networth`,
#' Mdl.0 = CCP.RF$EstMdl$`D.0+networth`)
#' CCP.NoCCP.RF <- Combined_Performance (Sub.CCP.RF)
#' }



Combined_Performance <- function(Sub.Est.Mdls){
  nmdls <- length(Sub.Est.Mdls)
  Pred_prob <- NULL
  for (ii in 1:nmdls) {
    Pred_prob <- rbind(Pred_prob, Sub.Est.Mdls[[ii]]$Pred_prob)
  }

  Actual <- NULL
  for (ii in 1:nmdls) {
    Actual <- as.matrix(rbind(Actual, Sub.Est.Mdls[[ii]]$Actual))
  }

  Combine.Mdl <- list(Pred_prob = Pred_prob)
  Combine.Mdl$Actual <- factor(Actual)

  Combine.Mdl$Roc <- multiclass.roc(Combine.Mdl$Actual, Pred_prob)

  ### Confusion Matrix
  Predicted_class <- NULL
  for (ii in 1:nmdls){
    Predicted_class <-  rbind(Predicted_class, as.matrix(Sub.Est.Mdls[[ii]]$Predicted_class))
  }

  Combine.Mdl$Predicted_class <- Predicted_class
  Combine.Mdl$ConfMat <- table(Combine.Mdl$Predicted_class, Combine.Mdl$Actual)

  # Model accuracy
  Combine.Mdl$ACC <- mean(Combine.Mdl$Predicted_class == Combine.Mdl$Actual, na.rm=T)

  return(Combine.Mdl)
}

#Sys.setenv('PATH' = paste0('C:/Program Files/qpdf-7.0.0-bin-msvc64/qpdf-7.0.0/bin;', Sys.getenv('PATH')))

# m2.xvar0 <- c("sex","married","age","havejob","educ","rural","region","income")
# m2.xvar1 <- c(m2.xvar0, "networth")   # with "networth"
# CCP <- sample_data %>% filter(sample_data$political.afl==1)
# NoCCP <- sample_data %>% filter(sample_data$political.afl==0)
# CCP.RF <-  RF_Model(CCP, m2.xvar1, yvar)
# NoCCP.RF <-  RF_Model(NoCCP, m2.xvar1, yvar)
# Sub.CCP.RF <- list(Mdl.1 = CCP.RF,  Mdl.2= NoCCP.RF)
# CCP.NoCCP.RF <- Combined_Performance(Sub.CCP.RF)
# CCP.NoCCP.RF$Roc$auc
