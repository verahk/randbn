#' Construct a bn.fit object
#'
#' Construct a [bnlearn::bn.fit] object from a DAG and a distribution (set of local distributions).
#'
#'
#' @param dag adjacency matrix
#' @param dist list of local distibution
#' @param use_bnlearn use [bnlearn::custom.fit()] to construct the bn.fit object (implies a set of checks of the input) or hardcode such an object (with no checks).
#'
#' @return an object of class [bnlearn::bn.fit]
#' @export
#'
#' @examples
custom_bn <- function(dag, dist, use_bnlearn = TRUE) {
  n <- ncol(dag)
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
