#' Wrapper around rBeta2009::rdirichlet that always return an n-by-k matrix
#'
#' @param n sample size
#' @param alpha hyperparameters
#' @param k number of outcomes ( = `length(alpha)`)
#' @export
rDirichlet <- function(n, alpha, k = length(alpha)){
  if (k == 2){
    p <- matrix(numeric(), n, k)
    p[, 1] <- rBeta2009::rbeta(n, alpha[1], alpha[2])
    p[, 2] <- 1-p[, 1]
  } else {
    p <- matrix(rBeta2009::rdirichlet(n, alpha), n, k)
  }
  
  # normalize 
  p <- p/rowSums(p)
  return(p)
}
