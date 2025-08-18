#' Draw a random distribution over a DAG
#'
#' Draw a random distribution over a DAG.
#' The function returns a list of CPTs (if `type = "categorical") stored
#' in a format matching [bnlearn::bn.fit] class and can be used to construct
#' such objects.
#'
#' @param dag (integer matrix) an adjacency matrix
#' @param type (character) type of distribution. Ignored if `rand_local_dist` is not `NULL`.
#' @param rand_local_dist (function) a function with arguments `j, parentnodes, args` that generates a random distribution.
#'  If `NULL` (default), and `type == categorical` then the function `rand::rand_cpt` is used.
#' @param ... additional arguments for the function producing each local distribution.
#'  See [rand_cpt()] for categorical distributions.
#'
#' @return a list with distributions
#' @export
#'
#' @seealso [rand_cpt()]
#' @examples
#'
#' dag  <- matrix(0, 3, 3, dimnames = list(c("Z", "X", "Y"), c("Z", "X", "Y")))
#' dag[upper.tri(dag)] <- 1
#' rand_dist(dag, "cat", nlev = rep(3, 3))
#'
rand_dist <- function(dag, type = "cat", rand_local_dist = NULL, ...) {
  type <- match.arg(type, c("categorical"))

  n <- ncol(dag)
  seqn <- seq_len(n)
  varnames <- colnames(dag)

  # specify default params and a function to draw local distribution
  args <- list(...)
  if (is.null(rand_local_dist) && type == "categorical") {
    rand_local_dist <- function(j, parentnodes, args) {
      if (is.null(args$nlev)) stop("Add argument `nlev` specifying the cardinality of each node.")
      args$nlev  <- args$nlev[c(j, parentnodes)]
      args$scope <- varnames[c(j, parentnodes)]
      do.call(rand_cpt, args)
    }
  }

  dist <- setNames(vector("list", n), varnames)
  for (j in seqn) {
    parentnodes <- seqn[dag[, j] == 1]
    dist[[j]] <- rand_local_dist(j, parentnodes, args)
  }
  dist
}
