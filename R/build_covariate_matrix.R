#' Summarizes and standardizes covariates into a matrix

buildCovariateMatrix = function(modFrm, covariateFunctions){
  covList <- lapply(modFrm[-1], function(x, id){
    unstack(data.frame(x, id), form = x ~ id)
  }, id = modFrm[[1]][,3])
  if(length(covList) == 0){
    return(NULL)
  }
  if(length(covariateFunctions) > 1){
    covList <- lapply(1:length(covList), function(i, cl, covFun){
      apply(cl[[i]], 2, covFun[i])
    }, cl = covList, covFun = covariateFunctions)
  }
  else{
    covList <- lapply(1:length(covList), function(i, cl, covFun){
      apply(cl[[i]], 2, covFun)
    }, cl = covList, covFun = covariateFunctions)
  }
  
  covMat <- as.matrix(do.call(cbind, covList))
  t(scale(covMat))
}
