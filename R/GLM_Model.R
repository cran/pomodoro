
#' Title
#'
#' @param Data The name of the Dataset.
#' @param xvar X variables.
#' @param yvar Y variable.
#' @return The output from  \code{\link{GLM_Model}}.
#' @export
#' @importFrom  caret createDataPartition
#' @importFrom  caret trainControl
#' @importFrom  caret train
#' @importFrom  pROC multiclass.roc
#' @importFrom stats glm
#' @importFrom stats binomial pnorm predict
#' @examples
#' yvar <- c("multi.level")
#' sample_data <- sample_data[c(1:750),]
#' m2.xvar0 <- c("sex", "married", "age", "havejob", "educ", "rural", "region","income")
#' BchMk.GLM <- GLM_Model(sample_data, c(m2.xvar0, "political.afl", "networth"), yvar)
#' BchMk.GLM$finalModel
#' BchMk.GLM$Roc$auc





GLM_Model <- function(Data, xvar, yvar){ # #' @export was deleted

  if (yvar == "Loan.Type"){
    Data.sub <- Data[, c(xvar, yvar)]
    Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c( "No.Loan", "Formal",  "Informal", "L.Both"))
  } else if(yvar == "multi.level"){
    Data.sub <- Data[, c(xvar, yvar)]
    Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c( "zero", "one"))
  }

  #set.seed(87)
  #Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c( "zero", "one"))
  train.set <-  createDataPartition(Data.sub[, yvar], p = .80, list = 0)
  Data.sub.train <- Data.sub[ train.set, ]
  Data.sub.test  <- Data.sub[-train.set, ]

  X.train <- Data.sub.train[ ,xvar]
  X.test  <- Data.sub.test[ ,xvar]
  Y.train <- Data.sub.train[ ,yvar]
  Y.test  <- Data.sub.test[ ,yvar]

  # Fit the model
  #myControl <- trainControl(method = "cv", number=10, summaryFunction = twoClassSummary, classProbs = TRUE,  verboseIter = TRUE)

  # Fit the model
  myControl <- trainControl("cv", 10,  verboseIter = TRUE)


  Est.GLM <- train(x = X.train, y = Y.train,  method = "glm", family=binomial, trControl = myControl,
                   preProcess = c("center", "scale"))

  glm.fit <- glm(Data.sub.train[,yvar] ~ ., data = Data.sub.train, family=binomial(link="logit"))

  Est.GLM$finalModel$call <- glm.fit$call

  # calculate Z score and p-Value for the variables in the model.
  z <- summary(Est.GLM)$coefficients/summary(Est.GLM)$standard.errors
  p <- (1 - pnorm(abs(z), 0, 1))*2

  Est.GLM$z <- z
  Est.GLM$pval <- p

  # Make predictions
  Pred.prob <- predict(Est.GLM, newdata = Data.sub.test, type = "prob")
  Roc  <- multiclass.roc(Y.test, Pred.prob)

  #colMeans(colAUC(Pred.prob, Data.sub.test[["multi.level"]]))

  Est.GLM$Pred_prob <- Pred.prob
  Est.GLM$Actual <- as.matrix(Y.test)
  Est.GLM$Roc <- Roc

  ### Confusion Matrix
  Est.GLM$Predicted_class <- predict(Est.GLM, newdata = Data.sub.test)
  Est.GLM$ConfMat <- table(Est.GLM$Predicted_class, Y.test)

  # Model accuracy
  Est.GLM$ACC <- mean(Est.GLM$Predicted_class == Y.test, na.rm=T)

  return(Est.GLM)

}







