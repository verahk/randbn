#' Draw a random distribution over a DAG
#'
#' Draw a random distribution over a DAG.
#' The function returns a list of CPTs (if `type = "categorical") stored
#' in a format matching [bnlearn::bn.fit] class and can be used to construct
#' such objects.
#'
#' @param dag (integer matrix) an adjacency matrix
#' @param type (character) type of distribution. 
#' @param nlev,levels specifies the cardinality of each variable, as a vector or 
#' a list with outcomes for each node, respectively. Used to set dimension and
#' dimmension names for each CPT, aligned with the requirements for bn.fit.dnode objects.   
#' If `levels = NULL`, the levels of node j is sat to `0, 1, ..., nlev[j]-1`.
#' Both arguments can not be `NULL`.
#' @param rand_cpt (function) a function that generates a CPT. 
#' The first argument must be the dimension of the CPT, derived from `nlev`, i.e.
#' `nlev[c(j, parentnodes)]` for node `j` with parents `parentnodes`.
#' @param 
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
#' rand_dist(dag, "cat", nlev = rep(3, 3), alpha = 1)
#' rand_dist(dag, "cat", nlev = rep(3, 3), rand_cpt = rand_cpt_cm, ess = 1000)


rand_dist <- function(dag, type = "cat", ...) {
  args <- list(...)
  switch(type, 
         "cat" = rand_dist_cat(dag, ...))
}

#' @rdname rand_dist
#' @export 
rand_dist_cat <- function(dag, nlev = lengths(levels), levels = NULL, rand_cpt = randbn::rand_cpt, ...) {
  
  n <- ncol(dag)
  seqn <- seq_len(n)
  
  if (length(nlev) != n) stop("argument `nlev` must be of length 1 or `ncol(dag)`")

  
  # init named list in which to store local distributions
  if (is.null(colnames(dag))) colnames(dag) <- paste0("X", seqn)
  dist <- stats::setNames(vector("list", n), colnames(dag))
  
  # dimnames 
  if (is.null(levels)) {
    if (is.null(nlev)) stop("Arguments must include cardinality of each node, specified as a list `levels` or a vector `nlev`.")
    levels <- lapply(nlev-1, seq.int, from = 0)
  }
  names(levels) <- colnames(dag)
    
  # iterate over each node and generate local distributions
  for (j in seqn) {
    parentnodes <- seqn[dag[, j] == 1]
    nodes  <- c(j, parentnodes)
    dimnms <- levels[nodes]
    dims   <- nlev[nodes]

    dist[[j]] <- array(rand_cpt(dims, ...), dims, dimnms)
  }
  return(dist)
}