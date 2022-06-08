#' Functional Synthetic Control Method
#'
#' @param formula formula
#' @param data data
#' @param covariateFunctions named list of function names for summarizing 
#'                           covariates. List names should correspond to
#'                           variable names.  Alternatively, instead of a list, 
#'                           a single function may be supplied to be used on 
#'                           all covariates.
#' @param cfArgs arguments for the covariate summary function
#' @param fpcaOptions options passed to FPCA()
#' @param V Either a numeric vector or matrix, or one of "minMSE" for
#'          automated selection of V or "FVE" for percent variation explained.
#' @param y outcome
#' @param time time
#' @param unit unit
#' @param intervention indicator for intervention
#' @param treated indicator for treated
#' @param x covariates
#' @param ... additional arguments passed to ipop() and optim().
#' 
#' @return funkySynth object
#' 
#' @details TODO
#'
#' @importFrom fdapace FPCA
#' @export

funcSynth = function(formula, data, covariateFunctions = "mean", cfArgs = NULL,
                     fpcaOptions = list(pre = list(), post = list()),
                     V = c("FVE", "minMSE"),
                     y = NULL, time = NULL, unit = NULL, intervention = NULL, 
                     treated = NULL, x = NULL, na.action = na.pass, ...){
  mCall <- match.call()
  mCallNoDots <- match.call(expand.dots = FALSE)
  # m <- match(x = c("formula", "data", "subset", "weights", "na.action", "offset"), 
  #            table = names(mf), nomatch = 0L)
  
  #TODO remove fpca options from matchDots function
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
  # minTime <- min(modFrm[[1]][,2])
  # maxTime <- max(modFrm[[1]][,2])
  
  
  listDat <- listifyData(modFrm[[1]])
  
  fpcaPre <- FPCA(listDat$ylistPre, listDat$tlistPre, fpcaOptions$pre)
  if(any(sapply(fpcaPre$xiVar, function(x){any(is.na(x))}))){
    fpcaOptions$pre$methodXi <- "CE"
    fpcaPre <- FPCA(listDat$ylistPre, listDat$tlistPre, fpcaOptions$pre)
  }
  xiPre <- fpcaPre$xiEst
  phiPre <- fpcaPre$phi
  muPre <- fpcaPre$mu
  lambdaPre <- fpcaPre$lambda
  fpcaDatPre <- phiPre %*% t(xiPre) + muPre
  
  #TODO Add is additional arguments to covariate summary functions from cfArgs
  covMat <- buildCovariateMatrix(modFrm, covariateFunctions)
  if(is.null(covMat)){
    X1 <- matrix(t(xiPre)[, treatedID], ncol = length(treatedID))
    X0 <- t(xiPre)[, -treatedID]
  }
  else{
    X1 <- rbind(matrix(t(xiPre)[, treatedID], ncol = length(treatedID)), 
                matrix(covMat[, treatedID], ncol = length(treatedID))) 
    X0 <- rbind(t(xiPre)[, -treatedID], covMat[, -treatedID])
  }
  # V <- lambdaPre / sum(lambdaPre)
  
  #TODO What to do about V when we have covariates?
  # nLambda = length(V)
  # nCov = length(modFrm) - 1
  # V <- c(V * (nLambda/(nLambda + nCov)), rep(1/(nLambda + nCov), times = nCov))
  
  #TODO add additional arguments for optimization (maybe using ...?)
  wV <- solveForWeights(X0, X1, V, Z0 = fpcaDatPre[,-treatedID],
                       Z1 = fpcaDatPre[,treatedID], lambda = fpcaPre$lambda)
  w <- wV$w
  V <- wV$V
  
  fpcaPost <- FPCA(listDat$ylistPost, listDat$tlistPost, fpcaOptions$post)
  if(any(sapply(fpcaPost$xiVar, function(x){any(is.na(x))}))){
    fpcaOptions$post$methodXi <- "CE"
    fpcaPost <- FPCA(listDat$ylistPost, listDat$tlistPost, fpcaOptions$post)
  }
  xiPost <- fpcaPost$xiEst
  phiPost <- fpcaPost$phi
  muPost <- fpcaPost$mu
  fpcaDatPost <- phiPost %*% t(xiPost) + muPost
  
  
  workGrid <- c(fpcaPre$workGrid, fpcaPost$workGrid)
  nPreGrid <- length(fpcaPre$workGrid)
  # nPostGrid = length(fpcaPost$workGrid)
  preTimes <- fpcaPre$obsGrid
  postTimes <- fpcaPost$obsGrid
  
  synthControl <- c(fpcaDatPre[, -treatedID] %*% w, fpcaDatPost[, -treatedID] %*% w)
  fpcaControls <- rbind(fpcaDatPre[,-treatedID], fpcaDatPost[,-treatedID])
  fpcaTreated <- c(fpcaDatPre[,treatedID], fpcaDatPost[,treatedID])
  RMSEpreTypeI <- sqrt(mean((listDat$ylistPre[[treatedID]] - synthControl[workGrid %in% preTimes])^2))
  RMSEpreTypeII <- sqrt(mean((fpcaTreated[1:nPreGrid] - synthControl[1:nPreGrid])^2))
  RMSPEpost <- sqrt(mean((listDat$ylistPost[[treatedID]] - synthControl[workGrid %in% postTimes])^2))
  #TODO create funkySynth class, also figure out what stats we should return
  
  out <- list(weights = w, V = V, call = mCall, data = data, 
             RMSPEpost = RMSPEpost, 
             RMSE = c(RMSEtypeI = RMSEpreTypeI, RMSEtypeII = RMSEpreTypeII),
             syntheticControl = synthControl, 
             functionalTreated = fpcaTreated,
             functionalControls = fpcaControls,
             fpca = list(pre = fpcaPre, post = fpcaPost),
             treatedID = treatedID)
  class(out) <- "funcSynth"
  
  return(out)
}
