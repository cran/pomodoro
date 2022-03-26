#' Bagging Model
#'
#' @details Decision trees suffer from high
#' variance (If we split the training data-set randomly into two parts and set a decision tree to both parts, the results might be quite different).
#' Bagging is an ensemble procedure which reduces the variance and increases the prediction accuracy of a statistical learning method
#' by considering many training sets
#' (\eqn{\hat{f}^{1}(x),\hat{f}^{2}(x),\ldots,\hat{f}^{B}(x)})
#' from the population. Since we can not have multiple training-sets, from a single training data-set, we can generate
#' \eqn{B}{B} different bootstrapped training data-sets
#' (\eqn{\hat{f}^{*1}(x), \hat{f}^{*2}(x), \ldots,\hat{f}^{*B}(x)})
#' by each \eqn{B}{B} trees and take a majority vote. Therefore, bagging for classification problem  defined as
#'  \deqn{\hat{f}(x)=arg\max_{k}\hat{f}^{*b}(x)}
#'
#'
#' @param Data The name of the Dataset.
#' @param xvar X variables.
#' @param yvar Y variable.
#' @return The output from  \code{\link{BAG_Model}}.
#' @export
#' @importFrom  gbm gbm.fit
#' @importFrom  ipred bagging
#' @importFrom stats binomial pnorm predict
#' @importFrom  caret createDataPartition
#' @importFrom  caret trainControl
#' @importFrom  caret train
#' @importFrom  pROC multiclass.roc
#' @examples
#' \donttest{
#' yvar <- c("Loan.Type")
#' sample_data <- sample_data[c(1:750),]
#' xvar <- c("sex", "married", "age", "havejob", "educ", "political.afl",
#' "rural", "region", "fin.intermdiaries", "fin.knowldge", "income")
#' BchMk.BAG <- BAG_Model(sample_data, c(xvar, "networth"), yvar )
#' BchMk.BAG$Roc$auc
#' }




BAG_Model <- function(Data, xvar, yvar){


  if (yvar == "Loan.Type"){
    Data.sub <- Data[, c(xvar, yvar)]
    Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c( "No.Loan", "Formal",  "Informal", "L.Both"))

  } else if(yvar == "multi.level"){
    Data.sub <- Data[, c(xvar, yvar)]
    Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c("zero", "one"))

  }

  train.set <- createDataPartition(Data.sub[, yvar], p=.80,list=0)
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
