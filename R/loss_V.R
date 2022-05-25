#' Loss function for optimizing V matrix
#' 
#' @param V V matrix for determining weights
#' @param X0 FPCA score and covariate matrix for controls
#' @param X1 FPCA score and covariate matrix for the treatment
#' @param Z0 Outcome matrix for the controls
#' @param Z1 Outcome matrix for the treatment
#' @param margin.ipop how close we get to the constraints; passed to ipop()
#' @param sigf.ipop Precision; passed to ipop()
#' @param bound.ipop Clipping bound for the variables; passed to ipop()

lossV = function(V, X0, X1, Z0, Z1, margin.ipop = 5e-04, sigf.ipop = 5,
                 bound.ipop = 10){
  w <- optimizeWeights(X0, X1, V, margin.ipop, sigf.ipop, bound.ipop)
  X0.scaled <- scale(X0)
  X1.scaled <- scale(X1)
  # loss.w <- as.numeric(t(X1.scaled - X0.scaled %*% w) %*% 
  #                        (V) %*% (X1.scaled - X0.scaled %*% w))
  loss.v <- as.numeric(t(Z1 - Z0 %*% w) %*% (Z1 - Z0 %*% w)) / nrow(Z0)
  return(loss.v)
}
