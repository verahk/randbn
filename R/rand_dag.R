#' Draw a random adjacency matrix
#'
#' Wrapper around [pcalg::randDAG] returning a adjacency matrix rather than a graphNEL object
#'
#' @param n (integer) number of nodes
#' @param d (integer) expected number of neighbours
#' @param varnames (character vector) node names. Defaults to `c("X1", "X2, ..., "Xn")`.
#' @param weighted (boolean) wheter each edge should be assigned weights. Default is FALSE.
#'
#' @return a n-by-n adjacency matrix where element `(i, j)` is 1 if there is an edge from node `i` to `j` and 0 otherwise.
#' @export
rand_dag <- function(n, d, varnames = NULL, weighted = FALSE) {
  g <- pcalg::randDAG(n, d, weighted = weighted)
  dag <- adjmat_from_graph(g)
  if (is.null(varnames)) varnames <- paste0("X", seq_len(n))
  colnames(dag) <- rownames(dag) <- varnames
  dag
}

adjmat_from_graph <- function(obj) {
  nodes <- obj@nodes
  edgeL <- obj@edgeL
  n <- length(nodes)
  mat <- matrix(0, n, n)
  dimnames(mat) <- list(nodes, nodes)

  for (node in nodes) {
    children <- edgeL[[node]]$edges
    if (length(children) > 0) mat[node, children] <- 1
  }
  return(mat)
}
