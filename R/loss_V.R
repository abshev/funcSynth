

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
