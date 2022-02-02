#` Functional Key Synthetic Control
#`
#' @importFrom fdapace FPCA
#' @importFrom Synth synth

funkySynth = function(y, t, unit, intervention, treated, ...){
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
  
  X1 = matrix(t(xiPre)[, treatedID], ncol = 1)
  X0 = t(xiPre)[, -treatedID]
  Z1 = matrix(fpcaDatPre[, treatedID], ncol = 1)
  Z0 = fpcaDatPre[, -treatedID]
  V = lambdaPre / sum(lambdaPre)
  
  synthFit = synth(X1 = X1, X0 = X0, Z1 = Z1, Z0 = Z0, custom.v = V)
  w = synthFit$solution.w
  
  fpcaPost = FPCA(listDat$ylistPost, listDat$tlistPost, fpcaOptions)
  xiPost = fpcaPost$xiEst
  phiPost = fpcaPost$phi
  muPost = fpcaPost$mu
  fpcaDatPost = phiPost %*% t(xiPost) + muPost
  synthControl = fpcaDatPost[, -treatedID] %*% w
  # RMSPEpre = sqrt(mean((fpcaDatPost[minTime:(interventionTime - 1), treatedID] - 
  #                         synthControl[minTime:(interventionTime - 1)])^2))
  # RMSPEpost = sqrt(mean((fpcaDatPost[interventionTime:maxTime, treatedID] - 
  #                          synthControl[interventionTime:maxTime])^2))
  # ATE = mean(fpcaDatPost[interventionTime:maxTime, treatedID] -
  #              synthControl[interventionTime:maxTime])
  ATE = mean(fpcaDatPost[, treatedID] -
               synthControl)
  return(list(#RMSPEpre = RMSPEpre, RMSPEpost = RMSPEpost, 
    ATE = ATE, w = w, 
    sytheticControl = synthControl, functionalData = fpcaDatPost,
    loss = synthFit$loss.w))
}
