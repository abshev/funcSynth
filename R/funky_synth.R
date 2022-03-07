#' Functional Key Synthetic Control
#'
#' TODO documentation
#' @param y outcome
#' @param t time
#' @param unit unit
#' @param intervention indicator for intervention
#' @param treated indicator for treated
#' @param data name of data object
#' @return funkySynth object
#' 
#' @details TODO
#'
#' @example 
#' TODO Example code  
#'
#' @importFrom fdapace FPCA

funkySynth = function(y, t, unit, intervention, treated, data, ...){
  #TODO Data checks
  #TODO Make data argument work as intended
  #TODO Formula input either as an option or instead of y, t, unit, 
  #       intervention, treated arguments
  #TODO best way to input covariates as an argument?
  
  treatedID = which(unique(unit) %in% unique(unit[treated]))
  interventionTime = sum(intervention == FALSE) / length(unique(unit))
  minTime = min(t)
  maxTime = max(t)
  
  listDat = listifyData(y, t, unit, interventionTime)
  
  fpcaOptions = list(...)
  
  fpcaPre = FPCA(listDat$ylistPre, listDat$tlistPre, fpcaOptions)
  xiPre = fpcaPre$xiEst
  phiPre = fpcaPre$phi
  muPre = fpcaPre$mu
  lambdaPre = fpcaPre$lambda
  fpcaDatPre = phiPre %*% t(xiPre) + muPre
  
  #TODO create appropriate matricies with covariates
  X1 = matrix(t(xiPre)[, treatedID], ncol = 1)
  X0 = t(xiPre)[, -treatedID]
  V = lambdaPre / sum(lambdaPre)
  
  #TODO add additional arguments for optimization (maybe using ...?)
  w = solveForWeights(X0, X1, V)
  
  fpcaPost = FPCA(listDat$ylistPost, listDat$tlistPost, fpcaOptions)
  xiPost = fpcaPost$xiEst
  phiPost = fpcaPost$phi
  muPost = fpcaPost$mu
  fpcaDatPost = phiPost %*% t(xiPost) + muPost
  synthControl = fpcaDatPost[, -treatedID] %*% w
  # RMSEpre = sqrt(mean((fpcaDatPost[minTime:(interventionTime - 1), treatedID] - 
  #                         synthControl[minTime:(interventionTime - 1)])^2))
  # RMSEpost = sqrt(mean((fpcaDatPost[interventionTime:maxTime, treatedID] - 
  #                          synthControl[interventionTime:maxTime])^2))
  # ATE = mean(fpcaDatPost[interventionTime:maxTime, treatedID] -
  #              synthControl[interventionTime:maxTime])
  ATE = mean(fpcaDatPost[, treatedID] -
               synthControl)
  #TODO create funkySynth class, also figure out what stats we should return
  
  return(list(#RMSEpre = RMSEpre, RMSEpost = RMSEpost, 
    ATE = ATE, w = w, 
    sytheticControl = synthControl, functionalData = fpcaDatPost))
}
