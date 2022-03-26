#' Gradient Boosting Model
#'
#' @details Unlike bagging trees, boosting does not use bootstrap sampling,
#' rather each tree is fit using information from previous trees.
#' An event probability of stochastic gradient boosting model is given by
#' \deqn{\hat{\pi_i} = \frac{1}{1 + exp[-f(x)]^\prime}}
#'  where \eqn{f(x)} is in the range of \eqn{[-\infty,\infty]} and its initial estimate of the model is
#'  \eqn{f^{(0)}_i=log(\frac{\pi_{i}}{1-\pi_{i}})},
#'  where \eqn{\hat{\pi}} is the estimated sample proportion of a single class from the training set.
#'
#'
#' @param Data The name of the Dataset.
#' @param xvar X variables.
#' @param yvar Y variable.
#' @return The output from  \code{\link{GBM_Model}}.
#' @export
#' @importFrom  gbm gbm.fit
#' @importFrom  caret createDataPartition
#' @importFrom  caret trainControl
#' @importFrom  caret train
#' @importFrom  pROC multiclass.roc
#' @importFrom stats binomial pnorm predict
#' @examples
#' \donttest{
#' yvar <- c("Loan.Type")
#' sample_data <- sample_data[c(1:120),]
#' xvar <- c("sex", "married", "age", "havejob", "educ", "political.afl",
#' "rural", "region", "fin.intermdiaries", "fin.knowldge", "income")
#' BchMk.GBM <- GBM_Model(sample_data, c(xvar, "networth"), yvar )
#' BchMk.GBM$finalModel
#' BchMk.GBM$Roc$auc
#' }


GBM_Model <- function(Data, xvar, yvar){

  if (yvar == "Loan.Type"){
    Data.sub <- Data[, c(xvar, yvar)]
    Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c( "No.Loan", "Formal",  "Informal", "L.Both"))


    #set.seed(87)
    train.set <- createDataPartition(Data.sub[, yvar], p = .80, list = 0)
    Data.sub.train <- Data.sub[ train.set, ]
    Data.sub.test  <- Data.sub[-train.set, ]

    X.train <- Data.sub.train[ ,xvar]
    X.test  <- Data.sub.test[ ,xvar]
    Y.train <- Data.sub.train[ ,yvar]
    Y.test  <- Data.sub.test[ ,yvar]

    # Fit the model
    #myControl <- trainControl("cv", 10,  verboseIter = TRUE) #classProbs = TRUE, savePredictions=T)  #"cv" = cross-validation, 10-fold

    Model.GBM <- train(x = X.train,  y = Y.train,  method = "gbm", preProcess = c("center", "scale"))

    GBM.fit <- gbm.fit(X.train, Y.train, distribution = "multinomial" )

  } else if(yvar == "multi.level"){
    Data.sub <- Data[, c(xvar, yvar)]
    Data.sub[, yvar] <- factor(ifelse(Data.sub[, yvar] == "one", 1, 0))


    #set.seed(87)
    train.set <- createDataPartition(Data.sub[, yvar], p = .80, list = 0)
    Data.sub.train <- Data.sub[ train.set, ]
    Data.sub.test  <- Data.sub[-train.set, ]

    X.train <- Data.sub.train[ ,xvar]
    X.test  <- Data.sub.test[ ,xvar]
    Y.train <- Data.sub.train[ ,yvar]
    Y.test  <- Data.sub.test[ ,yvar]

    # Fit the model
    #myControl <- trainControl("cv", 10,  verboseIter = TRUE, classProbs = TRUE, savePredictions=T) #classProbs = TRUE, savePredictions=T)  #"cv" = cross-validation, 10-fold

    # Fit the model

    Model.GBM <- train(x = X.train, y = Y.train,  method = "gbm",
                       #trControl =  myControl,
                       preProcess = c("center", "scale"))

    GBM.fit <- gbm.fit(X.train, Y.train, distribution = "bernoulli") #distribution = "gaussian")


  }

  #RF.fit <- randomForest::randomForest(Data.sub.train[,yvar] ~ ., data = Data.sub.train) #, family=binomial(link="logit"))

  Model.GBM$finalModel$call <- GBM.fit$call

  # Make predictions
  Pred.prob <- predict(Model.GBM, newdata = Data.sub.test, type = "prob")
  Roc  <- multiclass.roc(Y.test, Pred.prob)

  Model.GBM$Pred_prob <- Pred.prob
  Model.GBM$Actual <- as.matrix(Y.test)
  Model.GBM$Roc <- Roc

  ### Confusion Matrix
  Model.GBM$Predicted_class <- predict(Model.GBM, newdata = Data.sub.test)
  Model.GBM$ConfMat <- table(Model.GBM$Predicted_class, Y.test)

  # Model accuracy
  Model.GBM$ACC <- mean(Model.GBM$Predicted_class == Y.test, na.rm=T)

  return(Model.GBM)
}
