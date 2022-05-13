


confint.funcSynth = function(funcSynthObj, alpha = 0.05, 
                             type = c("syntheticControl", "gap")){
  if(any(!(type %in% c("syntheticControl", "gap")))){
    stop("Invalid confidence interval type")
  }
  
  fpcaPre <- funcSynthObj$fpca$pre
  fpcaPost <- funcSynthObj$fpca$post
  w <- funcSynthObj$weights
  treated <- funcSynthObj$treatedID
  synthControl <- funcSynthObj$syntheticControl
  treatedFunc <- funcSynthObj$functionalTreated
  
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
  
  if("syntheticControl" %in% type){
    marginOfErrorPre <- sqrt(stats::qchisq(1 - alpha, Kpre) * varSynthPre)
    marginOfErrorPost <- sqrt(stats::qchisq(1 - alpha, Kpost) * varSynthPost)
    CIlower.sc = synthControl - c(marginOfErrorPre, marginOfErrorPost)
    CIupper.sc = synthControl + c(marginOfErrorPre, marginOfErrorPost)
    CI.sc = cbind(CIlower.sc, CIupper.sc)
  }
  else{
    CI.sc = NULL
  }
  if("gap" %in% type){
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
  if(length(type) == 1){
    out <- list(syntheticControl = CI.sc, gap = CI.gp)[[type]]
  }
  else{
    out <- list(syntheticControl = CI.sc, gap = CI.gp)
  }
  
  return(out)
}

