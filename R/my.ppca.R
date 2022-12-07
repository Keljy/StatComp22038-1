#' @title  my.ppca
#' @description give the covariance matrix,calculate the probabilistic principal component by MLE.
#' @param q numbers of PC
#' @param S the covariance matrix
#' @return the maximum-likelihood estimator for variance and the weight matrix
#' @examples
#' \dontrun{
#' library(elasticnet) 
#' data(pitprops)
#' my.ppca(q=3,S=pitprops)
#' }
#' @export
my.ppca <- function(q,S){
  Uq <- eigen(S)$vectors[,1:q]
  eigenval <- eigen(S)$values
  Dq <- diag(eigenval[1:q])
  d <- ncol(S)
  sigma2_ML <- sum(eigenval[-(1:q)])/(d-q)
  I <- diag(rep(1,q))
  matrix <- Dq-sigma2_ML*I
  lamda <- solve(eigen(matrix)$vectors)%*%matrix%*%(eigen(matrix)$vectors)
  lamda_sqrt <- diag(sqrt(diag(lamda)))
  Sigma_sqrt <- (eigen(matrix)$vectors)%*%lamda_sqrt%*%solve(eigen(matrix)$vectors)
  W_ML <- Uq%*%Sigma_sqrt
  for (i in 1:q) {
    W_ML[,i] <- W_ML[,i]/sqrt(sum(W_ML[,i]*W_ML[,i]))
  }
  result <- list(sigma2_ML=sigma2_ML,W_ML=W_ML)
  return(result)
}