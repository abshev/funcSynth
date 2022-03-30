#' Functional Synthetic Control Method
#'
#' TODO documentation
#' @param formula formula
#' @param data data
#' @param covariateFunctions
#' @param cfArgs
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

funcSynth = function(formula, data, covariateFunctions = "mean", cfArgs = NULL,
                     y = NULL, time = NULL, unit = NULL, intervention = NULL, 
                     treated = NULL, x = NULL, ...){
  mCall <- match.call()
  mCallNoDots <- match.call(expand.dots = FALSE)
  # m <- match(x = c("formula", "data", "subset", "weights", "na.action", "offset"), 
  #            table = names(mf), nomatch = 0L)
  
  matchedDots <- matchDots(list(...))
  
  #TODO Data checks  (these will now be done mostly in modularSyth function)
  checkNullInputs <- c(is.null(y), is.null(time), is.null(unit), 
                      is.null(intervention), is.null(treated), is.null(x))
  if(missing(formula) & any(checkNullInputs)){
    stop("You must input either a formula or all of the following: y, time, unit, intervention, treated")
  }
  else if(missing(formula)){
    #TODO stuff here for argument input of data
  }
  else{
    # if(missing(data)){
    #   fterms = terms(formula)
    # }
    # else{
    #   fterms = terms(formula, data = data)
    # }
    indx <- match(c("formula", "data", "subset", "na.action"),
                  names(mCall), nomatch = 0)
    tform <- mCallNoDots[c(1, indx)]
    tform[[1L]] <- quote(stats::model.frame)
    modFrm <- eval(tform, parent.frame())
  }
  
  treatedID <- which(unique(modFrm[[1]][,3]) %in% 
                       unique(modFrm[[1]][,3][modFrm[[1]][,5] == 1]))
  interventionTime <- sum(intervention == FALSE) / length(unique(unit))
  minTime <- min(modFrm[[1]][,2])
  maxTime <- max(modFrm[[1]][,2])
  
  
  listDat <- listifyData(modFrm[[1]])
  
  fpcaPre <- FPCA(listDat$ylistPre, listDat$tlistPre, matchedDots$fpcaOptions)
  xiPre <- fpcaPre$xiEst
  phiPre <- fpcaPre$phi
  muPre <- fpcaPre$mu
  lambdaPre <- fpcaPre$lambda
  fpcaDatPre <- phiPre %*% t(xiPre) + muPre
  
  #TODO Add is additional arguments to covariate summary functions from cfArgs
  covMat <- buildCovariateMatrix(modFrm, covariateFunctions)
  X1 <- rbind(matrix(t(xiPre)[, treatedID], ncol = length(treatedID)), 
             matrix(covMat[, treatedID], ncol = length(treatedID))) 
  X0 <- rbind(t(xiPre)[, -treatedID], covMat[, -treatedID])
  V <- lambdaPre / sum(lambdaPre)
  
  #TODO What to do about V when we have covariates?
  nLambda = length(V)
  nCov = length(modFrm) - 1
  V <- c(V * (nLambda/(nLambda + nCov)), rep(1/(nLambda + nCov), times = nCov))
  
  #TODO add additional arguments for optimization (maybe using ...?)
  w <- solveForWeights(X0, X1, V)
  
  fpcaPost <- FPCA(listDat$ylistPost, listDat$tlistPost, 
                   matchedDots$fpcaOptions)
  xiPost <- fpcaPost$xiEst
  phiPost <- fpcaPost$phi
  muPost <- fpcaPost$mu
  fpcaDatPost <- phiPost %*% t(xiPost) + muPost
  synthControl <- fpcaDatPost[, -treatedID] %*% w
  # RMSEpre = sqrt(mean((fpcaDatPost[minTime:(interventionTime - 1), treatedID] - 
  #                         synthControl[minTime:(interventionTime - 1)])^2))
  # RMSEpost = sqrt(mean((fpcaDatPost[interventionTime:maxTime, treatedID] - 
  #                          synthControl[interventionTime:maxTime])^2))
  # ATE = mean(fpcaDatPost[interventionTime:maxTime, treatedID] -
  #              synthControl[interventionTime:maxTime])
  ATE <- mean(fpcaDatPost[, treatedID] - synthControl)
  #TODO create funkySynth class, also figure out what stats we should return
  
  return(list(#RMSEpre = RMSEpre, RMSEpost = RMSEpost, 
    ATE = ATE, w = w, 
    sytheticControl = synthControl, functionalData = fpcaDatPost))
}
