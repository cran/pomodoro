#' Generalized Linear Model
#'
#' @details Let \bold{y} be a vector of response variable of accessing credit for each applicant
#' \eqn{n}{n}, such that \eqn{y_{i}=1}{y_{i} =  1}  if the applicant-\eqn{i}{i}
#' has access to credit, and zero otherwise. Furthermore, let
#' let \eqn{\bold{x} = x_{ij}},  where
#' \eqn{i=1,\ldots,n}{i=1,...,n} and \eqn{j=1,\ldots,p}{j=1,...,p} characteristics of the applicants.
#' The log-odds can be define as:
#'
#'  \deqn{log(\frac{\pi_{i}}{1-\pi_{i}}) = \beta_{0}+\bold{x}_{\bold{i}}\beta = \beta_{0}+\sum_{i=1}^{p}\beta_{i}\bold{x}_{i}}
#'
#'  \eqn{\beta_{0}}{\beta_{0}} is the intercept, \eqn{\beta = (\beta_{1},\ldots, \beta_{p})} is
#'  a \eqn{p} \eqn{x} \eqn{1} vector of coefficients and
#' 	     \eqn{\bold{x_{i}}}{x_{i}} is the \eqn{i_{th}}{i_{th}} row of  \bold{x}.
#'
#'
#' @param Data The name of the Dataset.
#' @param xvar X variables.
#' @param yvar Y variable.
#' @return The output from  \code{\link{GLM_Model}}.
#' @export
#' @importFrom caret createDataPartition
#' @importFrom caret trainControl
#' @importFrom caret train
#' @importFrom pROC multiclass.roc
#' @importFrom stats glm
#' @importFrom stats binomial pnorm predict
#' @examples
#' yvar <- c("multi.level")
#' sample_data <- sample_data[c(1:750),]
#' xvar <- c("sex", "married", "age", "havejob", "educ", "political.afl",
#' "rural", "region", "fin.intermdiaries", "fin.knowldge", "income")
#' BchMk.GLM <- GLM_Model(sample_data, c(xvar, "networth"), yvar )
#' BchMk.GLM$finalModel
#' BchMk.GLM$Roc$auc






GLM_Model <- function(Data, xvar, yvar){ # #' @export was deleted

  #if(yvar == "Loan.Type"){
   #Data.sub <- Data[, c(xvar, yvar)]
   #Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c( "No.Loan", "Formal",  "Informal", "L.Both"))
  #}else if(yvar == "multi.level"){

  if(yvar == "multi.level"){
    Data.sub <- Data[, c(xvar, yvar)]
    Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c("zero", "one"))
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







