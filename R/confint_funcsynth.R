#' Confidence interval method for funcSynth objects
#' 
#' @param object a functional synthetic control object
#' @param parm one (or both) of "syntheticControl", "gap". The former produces 
#'             a confidence interval for the snythetic control estimate and
#'             the latter produces a confidence interval for the effect
#'             estimate.
#' @param level level of confidence for the interval
#' @param ... not used
#' @method confint funcSynth
#' 
#' @export



confint.funcSynth = function(object, parm = c("syntheticControl", "gap"),
                             level = 0.95, ...){
  if(any(!(parm %in% c("syntheticControl", "gap")))){
    stop("Invalid confidence interval type")
  }
  
  alpha <- 1 - level
  fpcaPre <- object$fpca$pre
  fpcaPost <- object$fpca$post
  w <- object$weights
  treated <- object$treatedID
  synthControl <- object$syntheticControl
  treatedFunc <- object$functionalTreated
  
  phiPre <- fpcaPre$phi
  phiPost <- fpcaPost$phi
  
  varPre <- lapply(fpcaPre$xiVar, function(xiV, phi){
    eigenXiV <- eigen(xiV)
    eigenVectorsXiV <- Re(eigenXiV$vectors)
    eigenValuesXiV <- Re(eigenXiV$values)
    eigenValuesXiV[which(eigenValuesXiV < 0)] <- 0
    if (length(eigenValuesXiV) == 1) {
      omega <- eigenVectorsXiV * eigenValuesXiV * t(eigenVectorsXiV)
    }
    else {
      omega <- eigenVectorsXiV %*% diag(eigenValuesXiV) %*% t(eigenVectorsXiV)
    }
    diag(phi %*% omega %*% t(phi))
    
  }, phi = phiPre)
  varSynthPre <- do.call(rbind, varPre[-treated])
  varSynthPre <- t(w^2) %*% varSynthPre
  varGapPre <- varSynthPre + varPre[[treated]]
  
  varPost <- lapply(fpcaPost$xiVar, function(xiV, phi){
    eigenXiV <- eigen(xiV)
    eigenVectorsXiV <- Re(eigenXiV$vectors)
    eigenValuesXiV <- Re(eigenXiV$values)
    eigenValuesXiV[which(eigenValuesXiV < 0)] <- 0
    if (length(eigenValuesXiV) == 1) {
      omega <- eigenVectorsXiV * eigenValuesXiV * t(eigenVectorsXiV)
    }
    else {
      omega <- eigenVectorsXiV %*% diag(eigenValuesXiV) %*% t(eigenVectorsXiV)
    }
    diag(phi %*% omega %*% t(phi))
  }, phi = phiPost)
  varSynthPost <- do.call(rbind, varPost[-treated])
  varSynthPost <- t(w^2) %*% varSynthPost
  varGapPost <- varSynthPost + varPost[[treated]]
  
  Kpre <- nrow(fpcaPre$xiVar[[treated]])
  Kpost <- nrow(fpcaPost$xiVar[[treated]])
  
  if("syntheticControl" %in% parm){
    marginOfErrorPre <- sqrt(stats::qchisq(1 - alpha, Kpre) * varSynthPre)
    marginOfErrorPost <- sqrt(stats::qchisq(1 - alpha, Kpost) * varSynthPost)
    CIlower.sc = synthControl - c(marginOfErrorPre, marginOfErrorPost)
    CIupper.sc = synthControl + c(marginOfErrorPre, marginOfErrorPost)
    CI.sc = cbind(CIlower.sc, CIupper.sc)
  }
  else{
    CI.sc = NULL
  }
  if("gap" %in% parm){
    marginOfErrorPre <- sqrt(stats::qchisq(1 - alpha, Kpre) * varGapPre)
    marginOfErrorPost <- sqrt(stats::qchisq(1 - alpha, Kpost) * varGapPost)
    CIlower.gp = (treatedFunc - synthControl) - 
      c(marginOfErrorPre, marginOfErrorPost)
    CIupper.gp = (treatedFunc - synthControl) + 
      c(marginOfErrorPre, marginOfErrorPost)
    CI.gp = cbind(CIlower.gp, CIupper.gp)
  }
  else{
    CI.gp = NULL
  }
  if(length(parm) == 1){
    out <- list(syntheticControl = CI.sc, gap = CI.gp)[[parm]]
  }
  else{
    out <- list(syntheticControl = CI.sc, gap = CI.gp)
  }
  
  return(out)
}

