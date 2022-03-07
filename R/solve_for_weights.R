#' Find SCM weights using quadratic optimization
#'
#' @importFrom kernlab ipop
#' 
#'  

solveForWeights = function(X0, X1, V, Margin.ipop = 5e-04, Sigf.ipop = 5,
                           Bound.ipop = 10){
  X0.scaled = scale(X0)
  X1.scaled = scale(X1)
  V <- diag(V)
  H <- t(X0.scaled) %*% V %*% (X0.scaled)
  c <- -1 * c(t(X1.scaled) %*% V %*% (X0.scaled))
  A <- t(rep(1, length(c)))
  b <- 1
  l <- rep(0, length(c))
  u <- rep(1, length(c))
  r <- 0
  
  res <- ipop(c = c, H = H, A = A, b = b, l = l, u = u, 
              r = r, margin = Margin.ipop, maxiter = 1000, sigf = Sigf.ipop, 
              bound = Bound.ipop)
  solution.w <- as.matrix(primal(res))
}
