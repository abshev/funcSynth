#' Function that returns weights and determines V matrix if it is not supplied
#'
#' @param X0 FPCA score and covariate matrix for controls
#' @param X1 FPCA score and covariate matrix for the treatment
#' @param Z0 Outcome matrix for the controls
#' @param Z1 Outcome matrix for the treatment
#' @param V Matrix specificing importance of FPCA scores and covariates for
#'          determining weights.
#' @param margin.ipop how close we get to the constraints; passed to ipop()
#' @param sigf.ipop Precision; passed to ipop()
#' @param bound.ipop Clipping bound for the variables; passed to ipop()
#' @param optimMethod One of "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
#'                    "Brent"; passed to optim().  See optim() for more 
#'                    information.


solveForWeights = function(X0, X1, Z0 = NULL, Z1 = NULL, V, lambda = NULL,
                           margin.ipop = 5e-04, sigf.ipop = 5, bound.ipop = 10, 
                           optimMethod = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
                                           "Brent")){
  
  #Check if V is supplied
  if(is.character(V)){
    if(V[1] == "minMSE"){
      nvars = nrow(X0)
      #Set initial value for V with equal weight
      initialV = rep(1/nvars, times = nvars)
      #Optimx to find V
      rgV.optim <- optim(par = initialV, fn = lossV, gr = NULL,
                         method = optimMethod, hessian = FALSE, 
                         control = list(), 
                         X0 = X0, X1 = X1, Z0 = Z0, Z1 = Z1,
                         margin.ipop = margin.ipop, sigf.ipop = sigf.ipop, 
                         bound.ipop = bound.ipop)
      # if (verbose == TRUE) {
      #   print(rgV.optim.1)
      # }
      
      # Xall <- cbind(X1.scaled, X0.scaled)
      # Xall <- cbind(rep(1, ncol(Xall)), t(Xall))
      # Zall <- cbind(Z1, Z0)
      # Beta <- try(solve(t(Xall) %*% Xall) %*% t(Xall) %*% t(Zall), 
      #             silent = TRUE)
      # if (inherits(Beta, "try-error")) {
      #   rgV.optim <- rgV.optim.1
      # }
      # else {
      #   Beta <- Beta[-1, ]
      #   V <- Beta %*% t(Beta)
      #   SV2 <- diag(V)
      #   SV2 <- SV2/sum(SV2)
      #   rgV.optim.2 <- optimx(par = SV2, fn = fn.V, gr = NULL, 
      #                         hess = NULL, method = optimxmethod, itnmax = NULL, 
      #                         hessian = FALSE, control = list(kkt = FALSE, 
      #                                                         starttests = FALSE, dowarn = FALSE, all.methods = all.methods), 
      #                         X0.scaled = X0.scaled, X1.scaled = X1.scaled, 
      #                         Z0 = Z0, Z1 = Z1, quadopt = quadopt, margin.ipop = Margin.ipop, 
      #                         sigf.ipop = Sigf.ipop, bound.ipop = Bound.ipop)
      #   if (verbose == TRUE) {
      #     print(rgV.optim.2)
      #   }
      #   rgV.optim.2 <- collect.optimx(rgV.optim.2, "min")
      #   if (verbose == TRUE) {
      #     cat("\n Equal weight loss is:", rgV.optim.1$value, 
      #         "\n")
      #     cat("\n Regression Loss is:", rgV.optim.2$value, 
      #         "\n")
      #   }
      #   if (rgV.optim.1$value < rgV.optim.2$value) {
      #     rgV.optim <- rgV.optim.1
      #   }
      #   else {
      #     rgV.optim <- rgV.optim.2
      #   }
      # }
      # V <- abs(rgV.optim$par)/sum(abs(rgV.optim$par))
      V <- rgV.optim$par
      V[V < 0] = 0
      V <- V / sum(V)
      w <- optimizeWeights(X0, X1, V, margin.ipop, sigf.ipop, bound.ipop)
      return(list(V = V, w = w))
    }
    else if(V[1] == "VFE"){
      V <- lambda / sum(lambda)
      w <- optimizeWeights(X0, X1, V, margin.ipop, sigf.ipop, bound.ipop)
      return(list(V = V, w = w))
    }
    else{
      stop("Invalid value for V.  Enter \"minMSE\", \"VFE\", or a numeric matrix")
    }
  }
  
  else if(is.matrix(V) & is.numeric(V)){
    #TODO checks for V
    w <- optimizeWeights(X0, X1, V, margin.ipop, sigf.ipop, bound.ipop)
    return(list(V = V, w = w))
  }
  else{
    stop("Invalid value for V.  Enter \"minMSE\", \"VFE\", or a numeric matrix")
  }
}



