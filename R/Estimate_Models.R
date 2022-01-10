
#' Title
#' @param DataSet The name of the Dataset.
#' @param yvar Y variable.
#' @param exog is a vector to be subtract from the calculation.
#' @param xvec is a vector of the variables to be used.
#' @param xadd  is an additional vector to be used.
#' @param type  can be RF, GLM, MLM, BAG, and GBM.
#' @param dnames  is the unique values of exog.
#' @return The output from  \code{\link{Estimate_Models}}.
#' @export
#' @examples
#' \donttest{
#' sample_data <- sample_data[c(1:750),]
#' m2.xvar0 <- c("sex","married","age","havejob","educ","rural","region","income")
#' CCP.RF <- Estimate_Models(sample_data, yvar = c("Loan.Type"),
#' exog = "political.afl", xvec = m2.xvar0,
#' xadd = "networth", type = "RF", dnames = c("0","1"))
#' }


Estimate_Models <- function(DataSet, yvar, exog = NULL, xvec, xadd, type, dnames){

  Data.All <- DataSet

  if (is.null(exog)){
    data.split <- DataSet
  } else {
    data.split <- split(DataSet, DataSet[,exog])
  }

  if (is.null(xadd)){
    xvar.1 <- c(xvec)
  }

  names(data.split) <- paste("Data.V", names(data.split), sep = "")
  data.split.name <- c(as.character("Data.All"), as.character(names(data.split)))
  #list2env(data.split, environment())

  my.list <- list(Data.All)
  for(i in 1:length(data.split)){
    my.list[[i+1]] <- data.split[[i]]
  }
  names(my.list) <- data.split.name

  res1 <- split(DataSet, DataSet[,exog])
  names(res1) <- paste("D", names(res1), sep = ".")

  a <- NULL
  for(i in 1:length(res1)){
    a[i] <-  vector("list", 1)
  }
  names(a) <- paste("D", dnames, sep = ".");a
  a <- c(as.character("BchMk"), as.character(names(a)));a
  #list2env(a, environment())

  xvar.0 <- setdiff(xvec, exog)

  Mdl.names <- NULL
  k = 0
  EstMdl <- NULL

  if (type == "GLM"){
    for(i in 1:length(my.list)){

      for (j in 1:length(xadd)) {
        xvar.1 <- c(xvar.0, xadd[j])
        k = k + 1

        Mdl.names[k] <- paste(a[i], xadd[j], sep = "+")
        # GLM.EstMdl[[k]] <- c(i,j)
        EstMdl[[k]] <- GLM_Model(my.list[[i]], xvar.1, yvar)
      }
    }
  }


  if (type == "RF"){
    for(i in 1:length(my.list)){

      for (j in 1:length(xadd)) {
        xvar.1 <- c(xvar.0, xadd[j])
        k = k + 1

        Mdl.names[k] <- paste(a[i], xadd[j], sep = "+")
        EstMdl[[k]] <- RF_Model(my.list[[i]], xvar.1, yvar)
      }
    }
  }

  if (type == "MLM"){
    for(i in 1:length(my.list)){

      for (j in 1:length(xadd)) {
        xvar.1 <- c(xvar.0, xadd[j])
        k = k + 1

        Mdl.names[k] <- paste(a[i], xadd[j], sep = "+")
        EstMdl[[k]] <- MLM_Model(my.list[[i]], xvar.1, yvar)
      }
    }
  }


  if (type == "BAG"){
    for(i in 1:length(my.list)){

      for (j in 1:length(xadd)) {
        xvar.1 <- c(xvar.0, xadd[j])
        k = k + 1

        Mdl.names[k] <- paste(a[i], xadd[j], sep = "+")
        EstMdl[[k]] <- BAG_Model(my.list[[i]], xvar.1, yvar)
      }
    }
  }

  if (type == "GBM"){
    for(i in 1:length(my.list)){

      for (j in 1:length(xadd)) {
        xvar.1 <- c(xvar.0, xadd[j])
        k = k + 1

        Mdl.names[k] <- paste(a[i], xadd[j], sep = "+")
        EstMdl[[k]] <- GBM_Model(my.list[[i]], xvar.1, yvar)
      }
    }
  }


  names(EstMdl) <- Mdl.names
  output <- list(EstMdl = EstMdl)
  return(output)

} #CLOSE ALL THE FUNCTION
