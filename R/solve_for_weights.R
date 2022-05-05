#' Find SCM weights using quadratic optimization
#'
#' @importFrom kernlab ipop

solveForWeights = function(X0, X1, Z0 = NULL, Z1 = NULL, V, margin.ipop = 5e-04, 
                           sigf.ipop = 5, bound.ipop = 10, 
                           optimMethod = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
                                           "Brent")){
  #Check if V is supplied
  if(!is.null(V)){
    w <- optimizeWeights(X0, X1, V, margin.ipop, sigf.ipop, bound.ipop)
    return(list(V = V, w = w))
  }
  #If V is not supplied, determine V by optimizing over MSPE
  else{
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
    V <- abs(rgV.optim$par)/sum(abs(rgV.optim$par))
    w <- optimizeWeights(X0, X1, V, margin.ipop, sigf.ipop, bound.ipop)
    return(list(V = V, w = w))
  }
}



