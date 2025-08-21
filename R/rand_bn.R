
#' Draw a random Bayesian network
#'
#' Draw a random DAG and a distribution over the DAG, and store the BN as a `bnlearn::bn.fit` object.
#'
#' @param dag (integer matrix) adjacency matrix.
#' @param type (character) type of distribution.
#' @param ... additional arguments sent to [rand_dist()]
#'
#' @return a `bnlearn::bn.fit` object

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

custom_bn <- function(dag, dist, use_bnlearn = TRUE) {

  varnames <- colnames(dag)
  if (is.null(varnames)) {
    varnames <- paste0("X", seq_len(ncol(dag)))
  }

  if (use_bnlearn) {
    colnames(dag) <- rownames(dag) <- varnames
    names(dist) <- varnames
    bn <- bnlearn::empty.graph(colnames(dag))
    bnlearn::amat(bn) <- dag
    return(bnlearn::custom.fit(bn, dist))
  } else {
    bn <- stats::setNames(vector("list", n), varnames)
    for (j in seq_along(bn)) {
      node <- list(node = varnames[j],
                   parents = varnames[which(dag[, j] == 1)],
                   children = varnames[which(dag[, j] == 1)],
                   prob = dist[[j]])
      class(node) <- "bn.fit.dnode"
      bn[[j]] <- node
    }
    class(bn) <- c("bn.fit", "bn.fit.dnet")
    return(bn)
  }
}




