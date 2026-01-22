
#' Draw a random partition of an outcome space
#' @param method name of partitioning algorithm.
#' @param dims cardinality of the variables.
#' @param prob parameter controlling the size of the partition.
#' @param regular if `TRUE`, the partition is forced to be regular. See [bida::make_regular].
#' @return an vector of length `prod(dims)` assigning each joint outcome to a
#'  subset of the partition.
#' @export
#'
#' @examples
#'
#' set.seed(1)
#' dims <- c(2, 2, 2)
#' rand_partition("tree", dims, .75)
#'
#' # tree of given depth
#' rand_partition("tree", dims, 1, maxdepth = 1)
#' rand_partition("tree", dims, 1, maxdepth = .5)
#'
#' # tree of given depth, forced to be regular
#' rand_partition("tree", dims, 1, maxdepth = 1, regular = TRUE)
rand_partition <- function(method = "tree", 
                           dims,
                           prob,
                           regular = FALSE,
                           ...) {
  method <- match.arg(method, c("tree"))
  if (length(dims) < 2) {
    stop("Need levels of at least two variable to produce a partition.")
  } else if (method == "tree") {
    tree <- rand_tree(dims, p, branch = "", ...)
    if (regular) make_regular(tree, dims)
    P <- rules_from_tree(tree)
  }
  return(P)
}




rand_labels <- function(dims, lprob) {
  
  n  <- length(dims)      # number of nodes
  q  <- prod(dims)        # number of joint outcomes
  joint  <- seq_len(q)    # joint outcomes
  
  stride <- c(1, cumprod(dims[-n]))
  levels <- lapply(dims-1, seq.int, from = 0)
  conf   <- expand_grid_fast(levels)
  labels <- vector("list", length(dims))
  
  for (i in seq_len(n)) {
    if (runif(1) < lprob) {
      nlabels <- rbinom(1, q/dims[i]-1, lprob)
      contexts <- sample(joint[conf[, i] == 0], nlabels, FALSE)
      labels[[i]] <- conf[contexts, -i, drop = FALSE]
    }
  }
  labels
}


#' grow a tree that partition the space of `length(dims)` variables
#' @param splitprob (numeric constant) probability of splitting a node
#' @param doMerge (logical constant) randomly merge leaves in tree
#' @param nextsplitprob (function) manipulate `splitprob` for each new split
#' 
rand_partition_tree <- function(dims,
                                splitprob,
                                regular,
                                nextsplitprob = function(x) x,
                                mindepth = 1,
                                maxdepth = length(dims)) {
  
  n <- length(dims)
  if (mindepth%/%1 == 0) mindepth <- max(1, round(n*mindepth))
  if (maxdepth%/%1 == 0 || maxdepth == 1) maxdepth <- min(round(n*maxdepth), n)
  
  
  
  n <- length(dims)
  stride <- c(1, cumprod(dims[-length(dims)]))
  subset <- seq_len(prod(dims))-1
  vars <- seq_along(dims)
  tree   <- grow_tree(splitprob, vars, subset)
  P <- unlist_tree(tree)
  
  if (regular) {
    get_splitvars <- function(tree) {
      if (!is.null(tree$branch)) {
        c(tree$var, unlist(lapply(tree$branches, get_splitvars), recursive = FALSE))
      }
    }
    vars_in <- unique(get_splitvars(tree))
    for (x in setdiff(vars, vars_in)) {
      subset <- P[[1]]
      xval <- (subset%/%stride[[x]])%%dims[[x]]
      P <- c(P[-1], unname(split(subset, xval)))
    }
  }
  
  stopifnot(!is.null(P))
  stopifnot(length(unlist(P)) == prod(dims))
  return(P)
}


# define routine for growing a tree
  grow_tree <- function(splitprob, vars, subset) {
    
    if ( (n-length(vars) >= mindepth && n-length(vars) >= maxdepth) || runif(1) > splitprob) {
      return(list(subset = subset))
    }
    
    # draw a split variable
    x <- sample(vars, 1)
    
    # split the current subset by values of x
    xval <- (subset%/%stride[x])%%dims[x]
    new_subsets <- unname(split(subset, xval))
    
    # grow a new tree for each value of the split variable
    list(var = x,
         branches = lapply(new_subsets,
                           function(y) grow_tree(nextsplitprob(splitprob),
                                                 vars[!vars == x],
                                                 y)))
  }
  
  
  
  # define routine for extracting subsets in each leaf
  unlist_tree <- function(tree, name) {
    if (is.null(tree$branches)) unname(tree[name])
    else unlist(lapply(tree$branches, unlist_tree), recursive = FALSE)
  }
