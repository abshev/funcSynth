#' Find SCM weights using quadratic optimization
#'
#' @param X0 FPCA score and covariate matrix for controls
#' @param X1 FPCA score and covariate matrix for the treatment
#' @param V Matrix specificing importance of FPCA scores and covariates for
#'          determining weights.
#' @param margin.ipop how close we get to the constraints; passed to ipop()
#' @param sigf.ipop Precision; passed to ipop()
#' @param bound.ipop Clipping bound for the variables; passed to ipop()
#'
#' @importFrom kernlab ipop primal

optimizeWeights = function(X0, X1, V, margin.ipop = 5e-04, sigf.ipop = 5,
                           bound.ipop = 10){
  X0.scaled <- X0
  X1.scaled <- X1
  # X0.scaled <- scale(X0)
  # X1.scaled <- scale(X1)
  V <- diag(V)
  H <- t(X0.scaled) %*% V %*% (X0.scaled)
  cee <- -1 * c(t(X1.scaled) %*% V %*% (X0.scaled))
  A <- t(rep(1, length(cee)))
  bee <- 1
  l <- rep(0, length(cee))
  u <- rep(1, length(cee))
  r <- 0
  
  res <- ipop(c = cee, H = H, A = A, b = bee, l = l, u = u, 
              r = r, margin = margin.ipop, maxiter = 1000, sigf = sigf.ipop, 
              bound = bound.ipop)
  return(as.matrix(primal(res)))
}
