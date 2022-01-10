
#' Title
#'
#' @param Data The name of the Dataset.
#' @param xvar X variables.
#' @param yvar Y variable.
#' @return The output from  \code{\link{BAG_Model}}.
#' @export
#' @importFrom  gbm gbm.fit
#' @importFrom  ipred bagging
#' @importFrom stats binomial pnorm predict
#' @examples
#' \donttest{
#' yvar <- c("Loan.Type")
#' sample_data <- sample_data[c(1:750),]
#' m2.xvar0 <- c("sex", "married", "age", "havejob", "educ", "rural", "region","income")
#' BchMk.BAG <- BAG_Model(sample_data, c(m2.xvar0, "political.afl", "networth"), yvar)
#' BchMk.BAG$finalModel
#' BchMk.BAG$Roc$auc
#' }



BAG_Model <- function(Data, xvar, yvar){


  if (yvar == "Loan.Type"){
    Data.sub <- Data[, c(xvar, yvar)]
    Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c( "No.Loan", "Formal",  "Informal", "L.Both"))

  } else if(yvar == "multi.level"){
    Data.sub <- Data[, c(xvar, yvar)]

  }

  train.set <- createDataPartition(Data.sub$Loan.Type, p=.80,list=0)
  Data.sub.train <- Data.sub[ train.set, ]
  Data.sub.test  <- Data.sub[-train.set, ]

  X.train <- Data.sub.train[ , xvar]
  X.test  <- Data.sub.test[, xvar]
  Y.train <- Data.sub.train[ ,yvar]
  Y.test  <- Data.sub.test[ ,yvar]
  # Fit the model
  myControl <- trainControl("cv", 10,  verboseIter = TRUE) #classProbs = TRUE, savePredictions=T)  #"cv" = cross-validation, 10-fold

  Model.BAG <- train(x = X.train, y = Y.train,  method = "treebag",  trControl = myControl,
                     preProcess = c("center", "scale"))


  BAG.fit <- ipred::bagging(Y.train  ~ ., data = Data.sub.train)
  #BAG.fit <- ipred::bagging(X.train, Y.train )

  Model.BAG$finalModel$call <- BAG.fit$call

  # Make predictions
  Pred.prob <- predict(Model.BAG, newdata = Data.sub.test, type = "prob")
  Roc  <- multiclass.roc(Y.test, Pred.prob)

  Model.BAG$Pred_prob <- Pred.prob
  Model.BAG$Actual <- as.matrix(Y.test)
  Model.BAG$Roc <- Roc

  ### Confusion Matrix
  Model.BAG$Predicted_class <- predict(Model.BAG, newdata = Data.sub.test)
  Model.BAG$ConfMat <- table(Model.BAG$Predicted_class, Y.test)

  # Model accuracy
  Model.BAG$ACC <- mean(Model.BAG$Predicted_class == Y.test, na.rm=T)

  return(Model.BAG)
}
