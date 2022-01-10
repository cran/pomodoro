#' Title
#'
#' @param Data The name of the Dataset.
#' @param xvar X variables.
#' @param yvar Y variable.
#' @return The output from  \code{\link{MLM_Model}}.
#' @export
#' @importFrom  caret createDataPartition
#' @importFrom  caret trainControl
#' @importFrom  caret train
#' @importFrom  pROC multiclass.roc
#' @importFrom stats binomial pnorm predict
#' @examples
#' yvar <- c("Loan.Type")
#' sample_data <- sample_data[c(1:750),]
#' m2.xvar0 <- c("sex", "married", "age", "havejob", "educ", "rural", "region","income")
#' BchMk.MLM <- MLM_Model(sample_data, c(m2.xvar0, "political.afl", "networth"), yvar)
#' BchMk.MLM$finalModel
#' BchMk.MLM$Roc$auc



MLM_Model <- function(Data, xvar, yvar){
  # This can be implemented with mlogit() from mlogit package and multinom() from nnet package.

  #set.seed(87)
  Data <- as.data.frame(Data)
  Data.sub <- Data[, c(xvar, yvar)]
  #Data.sub$Loan.Type <- factor(Data.sub$Loan.Type)
  #Data.sub$Loan.Type = relevel(Data.sub$Loan.Type, ref =  "No.Loan")
  Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c( "No.Loan", "Formal",  "Informal", "L.Both"))


  # Split the data into training and test set
  #train.set <- createDataPartition(Data.sub$Loan.Type, p=.80,list=0)
  train.set <- createDataPartition(Data.sub[, yvar], p=.80,list=0)
  Data.sub.train <- Data.sub[ train.set, ]
  Data.sub.test  <- Data.sub[-train.set, ]

  X.train <- Data.sub.train[ , xvar]
  X.test  <- Data.sub.test[, xvar]
  Y.train <- Data.sub.train[ , yvar]
  Y.test  <- Data.sub.test[, yvar]
  #Y.train <- Data.sub.train$Loan.Type
  #Y.test  <- Data.sub.test$Loan.Type



  myControl <- trainControl("cv", 10,verboseIter = TRUE) # classProbs = TRUE, savePredictions=T)  #"cv" = cross-validation, 10-fold

  # Fit the model
  Est.MLR <- train(x=X.train, y=Y.train, method = "multinom",  trControl = myControl, preProcess = c("center", "scale"))


  #mlr.fit <- nnet::multinom(Loan.Type~., data = Data.sub.train) # multinom Model multi1 <- multinom(Loan.Type ~ .-1, data=Data.sub.train)


  #Est.MLR$finalModel$call <- mlr.fit$call


  # calculate Z score and p-Value for the variables in the model.
  z <- summary(Est.MLR)$coefficients/summary(Est.MLR)$standard.errors
  p <- (1 - pnorm(abs(z), 0, 1))*2

  Est.MLR$z <- z
  Est.MLR$pval <- p

  # Make predictions
  Pred.prob <- predict(Est.MLR, newdata = Data.sub.test, type = "prob")
  Roc  <- multiclass.roc(Y.test, Pred.prob)

  Est.MLR$Pred_prob <- Pred.prob
  Est.MLR$Actual <- as.matrix(Y.test)
  Est.MLR$Roc <- Roc

  ### Confusion Matrix
  Est.MLR$Predicted_class <- predict(Est.MLR, newdata = Data.sub.test)
  Est.MLR$ConfMat <- table(Est.MLR$Predicted_class, Y.test)

  # Model accuracy
  Est.MLR$ACC <- mean(Est.MLR$Predicted_class == Y.test, na.rm=T)

  return(Est.MLR)
  #output <- list(Est.MLR = Est.MLR,
  #               mlr.fit = mlr.fit)
  #return(output)
}
