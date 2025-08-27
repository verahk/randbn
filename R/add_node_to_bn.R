#' Add a new node to a Bayesian network
#' 
#' Augment a bn.fit object with a new node. Iterate trough all children of the 
#' new node and add local distributions for each configuration of the new node.
#' 
#' @param bn `bnlearn::bn.fit` object. Currently only discrete networks.
#' @param new_node  a `bnlearn::bn.fit.dnode` object
#' @param generate_local_dist function that generates a CPT. 
#'  First argument is assumed to be the dimension of the new rows in the CPT, 
#'  `c(r, q, s-1)`, where `r` is the cardinality of `child_node`, `q` the number 
#'  of configurations of the current parent set and `s` the cardinality of the new parent.
#'  By default, `generate_local_dist = rand_cpt`.
#' @param ... additional arguments to `generate_local_dist`.
#' @return 
#' `add_node_to_bn()` returns a [bnlearn::bn.fit] object, where `node` is added to the original object `bn`.
#' @export 
#' @example inst/example/example_add_node_to_bn.R
add_node_to_bn <- function(bn, new_node, generate_local_dist = randbn::rand_cpt, ...) {

  
  # check that node the additional node results in a valid DAG
  if (! check_new_node(bn, new_node)) return (NULL)
  
  # add node to new bn 
  new_bn <- unclass(bn)
  new_bn[[node]] <- new_node
  
  # add local distributions to every child of the new node
  for (child in new_node$children){
    new_bn[[child]] <- add_parent_to_child(new_bn[[child]], new_node, generate_local_dist, ...)
  }
  for (parent in new_node$parents) {
    new_bn[[parent]]$children <- c(new_bn[[parent]]$children, node)
  }
  # set class of augmented network equal to the original 
  class(new_bn) <- class(bn)
  return(new_bn)
}

check_new_node <- function(bn, node) {
  # name of node 
  stopifnot(is.character(bn$node))
  stopifnot(length(bn$node) == 1)
  stopifnot(match(bn$node, names(bn), 0L) == 0)
  
  # name of children
  stopifnot(is.character(bn$children))
  stopifnot(length(bn$children) == 0 || all(match(bn$children, names(bn), 0L) == 0))
  
  # name of parents 
  stopifnot(is.character(bn$parents))
  stopifnot(length(bn$parents) == 0 || all(match(bn$parents, names(bn), 0L) == 0))
  
  # check if augmented dag is valid 
  dag <- bnlearn::amat(bn)
  n <- ncol(dag)
  new_dag <- cbind(rbind(dag, 0), 0)
  colnames(new_dag) <- rownames(new_dag) <- c(colnames(dag), node$node)
  new_dag[node$parents, n+1] <- 1
  new_dag[n+1, node$children] <- 1
  return(pcalg::isValidGraph(t(new_dag), "dag", verbose = TRUE))
}



#' @rdname add_node_to_bn
#' @param child_node,parent_node `bn.fit.dnode` objects representing the child and its new parent.
#' @export 
#' @returns `add_parent_to_child()` returns a [bnlearn::bn.fit.dnode] object, where the set of local
#'  distributions is expanded to include also the new parent.
add_parent_to_child <- function(child_node, parent_node, generate_local_dist = randbn::rand_cpt, ...) {
  if (!inherits(child_node, "bn.fit.dnode")) {
    stop("Currently only supported for class `bn.fit.dnode`")
  }
  if (!child_node$node %in% parent_node$children) return(child_node)
  if (length(dim(child_node$prob)) == 1)  names(dimnames(child_node$prob)) <- child_node$node
  if (length(dim(parent_node$prob)) == 1) names(dimnames(parent_node$prob)) <- parent_node$node
  
  r <- dim(child_node$prob)[1]
  q <- prod(dim(child_node$prob)[-1])
  s <- dim(parent_node$prob)[1]
  
  # generate local distirbutions for the new parent-configurations
  new_rows <- generate_local_dist(c(r, q, s-1), ...)
  
  # combine with old CPT
  dims <- c(dim(child_node$prob), dim(parent_node$prob)[1]) 
  dimnms <- c(dimnames(child_node$prob), dimnames(parent_node$prob)[1])
  new_cpt <- array(c(child_node$prob, new_rows), dims, dimnms)
  
  # replace in node
  child_node$prob <- new_cpt
  child_node$parents <- names(dimnms)[-1]
  return(child_node)
}


#' @rdname add_node_to_bn
#' @param name name of new node
#' @param parents name of parents
#' @param children name of children
#' @param prob an array with named dimensions, representing the local distributions of the node.
#' @returns `new_node()` returns a [bnlearn::bn.fit.dnode] object
#' @export 
new_node <- function(name, parents = character(0), children = character(0), prob) {
  stopifnot(!is.null(dimnames(prob)))
  stopifnot(all(names(dimnames(prob)) == c(name, parents)))
  out <- list(node = name, 
              parents = parents, 
              children = children,
              prob = prob)
  class(out) <- "bn.fit.dnode"
  out
}



