#' Random Forest
#'
#' @details
#' Rather than considering the random sample of \eqn{m}{m} predictors
#' from the total of \eqn{p}{p} predictors in each split,
#' random forest does not consider a majority of the \eqn{p}{p} predictors, and considers in each split a
#' fresh sample of  \eqn{m_{try}}{m_{try}} which we usually set  to \eqn{m_{try} \approx \sqrt{p}}
#' Random forests which de-correlate the trees by considering  \eqn{m_{try} \approx \sqrt{p}}
#' show an improvement over bagged trees  \eqn{m = p}{m = p}.
#'
#' @param Data The name of the Dataset.
#' @param xvar X variables.
#' @param yvar Y variable.
#' @return The output from  \code{\link{RF_Model}}.
#' @export
#' @importFrom  pROC multiclass.roc
#' @importFrom  stats binomial pnorm predict
#' @importFrom  randomForest randomForest
#' @importFrom  caret createDataPartition
#' @importFrom  caret trainControl
#' @importFrom  caret train
#' @examples
#' \donttest{
#' sample_data <- sample_data[c(1:750),]
#' yvar <- c("Loan.Type")
#' xvar <- c("sex", "married", "age", "havejob", "educ", "political.afl",
#' "rural", "region", "fin.intermdiaries", "fin.knowldge", "income")
#' BchMk.RF <- RF_Model(sample_data, c(xvar, "networth"), yvar )
#' BchMk.RF
#'  }




RF_Model <- function(Data, xvar, yvar){ # #' @export was deleted


  if (yvar == "Loan.Type"){
    Data.sub <- Data[, c(xvar, yvar)]
    Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c( "No.Loan", "Formal",  "Informal", "L.Both"))
  } else if(yvar == "multi.level"){
    Data.sub <- Data[, c(xvar, yvar)]
    Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c( "zero", "one"))
  }

  #set.seed(87)
  train.set <- createDataPartition(Data.sub[, yvar], p = .80, list = 0)
  Data.sub.train <- Data.sub[ train.set, ]
  Data.sub.test  <- Data.sub[-train.set, ]

  X.train <- Data.sub.train[ ,xvar]
  X.test  <- Data.sub.test[ ,xvar]
  Y.train <- Data.sub.train[ ,yvar]
  Y.test  <- Data.sub.test[ ,yvar]

  # Fit the model
  myControl <- trainControl("cv", 10,  verboseIter = TRUE) #classProbs = TRUE, savePredictions=T)  #"cv" = cross-validation, 10-fold

  Model.RF <- train(x = X.train, y = Y.train,  method = "rf",  trControl = myControl,
                    preProcess = c("center", "scale"))

  RF.fit <- randomForest(Data.sub.train[,yvar] ~ ., data = Data.sub.train) #, family=binomial(link="logit"))

  Model.RF$finalModel$call <- RF.fit$call

  # Make predictions
  Pred.prob <- predict(Model.RF, newdata = Data.sub.test, type = "prob")
  Roc  <- multiclass.roc(Y.test, Pred.prob)

  Model.RF$Pred_prob <- Pred.prob
  Model.RF$Actual <- as.matrix(Y.test)
  Model.RF$Roc <- Roc

  ### Confusion Matrix
  Model.RF$Predicted_class <- predict(Model.RF, newdata = Data.sub.test)
  Model.RF$ConfMat <- table(Model.RF$Predicted_class, Y.test)

  # Model accuracy
  Model.RF$ACC <- mean(Model.RF$Predicted_class == Y.test, na.rm=T)
  return(Model.RF)
}

