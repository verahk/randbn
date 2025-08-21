
#' Draw a random Bayesian network
#'
#' Draw a random DAG and a distribution over the DAG, and store the BN as a `bnlearn::bn.fit` object.
#'
#' @inheritParams rand_dag
#' @inheritParams rand_dist
#' @inheritParams custom_bn
#' @param ... additional arguments sent to [rand_dist()]
#'
#' @return a `bnlearn::bn.fit` object
#' @export
#' @examples
#'
#' n <- 3
#' dag <- matrix(0, n, n)
#' dag[upper.tri(dag)] <- 1
#'
#'
#' # categorical ---
#' # draw random DAG
#' set.seed(007)
#' dag <- rand_dag(3, 2)
#' dag
#'
#' # draw random distrib over DAG
#' cpts <- rand_dist(dag, "cat", nlev = rep(3, n))
#'
#' # create bn object
#' bn <- custom_bn(dag, cpts)
#'
#' # replicate with rand_bn()
#' set.seed(007)
#' bn2 <- rand_bn(3, 2, "cat", nlev = rep(3, n))
#' stopifnot(all.equal(bn, bn2))
#'

rand_bn <- function(n, d, type = "cat", use_bnlearn = TRUE, ...) {
  dag <- rand_dag(n, d)
  dist <- rand_dist(dag, type, ...)
  custom_bn(dag, dist, use_bnlearn)
}







