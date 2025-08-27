#' Draw a conditional probability table (CPT)
#'
#' Construct a random conditional probability table (CPT) by sampling independently
#' a vector of probabilities from a Dirichlet distribution for each parent configuration.
#' If the scope of the CPT is specified, the function returns an array with
#' named dimensions, a format accepted for local distributions by [bnlearn::bn.fit.dnode].
#'
#' @param dims (integer vector) of cardinality of the outcome variable and the
#'  parent variables (the dimensions of the CPT). First element correspond to the outcome,
#'  the rest to its parents.
#' @param alpha (numeric vector) hyperparameters for the Dirichlet distribution(s)
#'  each parameter vector is drawn from.
#' @param ess (numeric constant) imaginary sample size 
#' @param dimnms (list) dimnames of CPT.
#' @param scope (character vector) names of the dimnames. If `scope` is specified and
#' `dimnms = NULL`, the dimnames for the `j`th margin is sat equal to `0, 1, .., dims[j]-1.`
#' @details
#' `rand_cpt()` assumes `alpha` is the hyperparameters of the Dirichlet distribution(s)
#' that each parameter vector in the CPT is sampled from. 
#' Each vector is sampled from
#' 
#' - a single, symmetric Dirichlet if `length(alpha) == 1`.
#' - a single, possible non-symmetric Dirichlet if `length(alpha) == dims[1]`.
#' - possibly distinct, non-symmetric Dirichlets if `length(alpha) == prod(dims)`.
#' 
#' Hence, one can specify the Dirichlet (for each parent configuration) also by 
#' a vector of means `mu` and an equivalent sample size `ess`, and call `rand_cpt(dims, alpha = mu*ess)`. 
#' `rand_cpt_cm()` does exactly this. The mean-vector is specified and peturbated
#' for each parent configuration as described in CM [TODO: add reference].
#' 
#' @return an array with dimension `dims` representing a CPT.
#' @export
#'
#' @examples
#'
#' # no parents
#' cpt <- rand_cpt(dims = 2, scope = "Y")
#' cpt
#' dimnames(cpt)
#'
#' # two parents
#' cpt <- rand_cpt(2:4, scope = c("Y", "X", "Z"))
#' cpt
#' 
#' # draw each vector from a skewed Dirichlet
#' cpt <- rand_cpt(2:4, alpha = c(.1, .9)*1000, scope = c("Y", "X", "Z"))
#' cpt
#' 
#' # cm-method with alternating means
#' cpt <- rand_cpt_cm(rep(2, 3), ess = 1000, scope = c("Y", "X", "Z"))
#' cpt  # every second parameter vector is drawn from the same Dirichlet
#'
#' # cm-method with randomly peturbed mean vector
#' cpt <- rand_cpt_cm(rep(2, 3), ess = 1000, shuffle = TRUE, scope = c("Y", "X", "Z"))
#' cpt  # every second parameter vector is drawn from the same Dirichlet
#' 
#' # with local structure
#' \dontrun{
#' cpt <- rand_cpt(dims, local_struct = "tree", prob = 1, maxdepth = .5)
#' }


rand_cpt <- function(dims,
                     alpha = 1,
                     dimnms = NULL,
                     scope = names(dimnames)) {

  r <- dims[1]
  q <- prod(dims[-1])

  # draw one Dirichlet vector for each parent config
  if (length(alpha) == 1) {
    p <- t(rDirichlet(q, rep(alpha, r), r))
  } else if (length(alpha) == r) {
    p <- t(rDirichlet(q, alpha, r))
  } else if (length(alpha) == q*r) {
    p <- vapply(split(alpha, rep(seq_len(q), each = r)),
                rDirichlet, n = 1, k = r,
                numeric(r))
  } else {
    stop("The hyperparamter alpha must be either length 1, dims[1] or prod(dims).")
  }
  
  
  # set dimension of array
  dim(p) <- dims

  # set dimnames if specified 
  if (!is.null(scope)) {
    if (is.null(dimnms)) {
      dimnms <- lapply(dims-1, seq.int, from = 0)
    }
    names(dimnms) <- scope
    dimnames(p) <- dimnms
  } 
  
  return(p)
}

#' @rdname rand_cpt 
#' @details 
#' `rand_cpt()` draw a vector from a single or a set of Dirichlet distribution as specified 
#' by `alpha`. 

#' @export 
rand_cpt_cm <- function(dims, ess = 10, shuffle = FALSE, dimnms = NULL, scope = names(dimnms)) {
  r <- dims[1]
  q <- prod(dims[-1])
  
  # create matrix with means of each outcome
  tmp <- 1/seq_len(r)
  mu <- matrix(tmp/sum(tmp), r, q)
  for (qq in seq_len(q)[-1]){
    mu[, qq] <- c(mu[r, qq-1], mu[-r, qq-1])
  }
  
  # shuffle rows in CPT
  indx <- seq_len(q)
  if (shuffle) indx <- sample(indx)
  mu <- mu[, indx]
  
  rand_cpt(dims, alpha = ess*mu, dimnms = dimnms, scope = scope)
}

#' @param local_struct (character) name of algorithm to form a partitioning /
#'  produce parameter restrictions.
#'  Defaults to `"none"`, which returns a CPT with no parameter restrictions.
#' @param ... additional arguments sendt to [rand_partition].
# rand_cpt_lstruct <- function() {
#   is_local_struct <- length(dims) > 2 && local_struct != "none"
#
#   if (is_local_struct) {
#     args <- list(...)
#     args$dims <- dims[-1]
#     args$method <- local_struct
#     print(args)
#     tmp <- do.call(rand_partition, args)
#     print(tmp)
#     q <- length(unique(tmp$parts))
#   } else {
#     q <- prod(dims[-1])
#   }
#
#   if (is_local_struct) {
#     p <- t(rDirichlet(length(tmp$partition), alpha, r))[, tmp$parts]
#     attr(p, "parts") <- tmp$parts
#   }
#
# }

