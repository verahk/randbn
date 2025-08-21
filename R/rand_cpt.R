#' Draw a conditional probability table (CPT)
#'
#' Construct a random conditional probability table (CPT) by sampling independently
#' a vector of probabilities from a Dirichlet distribution for each parent configuration.
#' If the scope of the CPT is specified, the function returns an array with
#' named dimensions, a format accepted for local distributions by [bnlearn::bn.fit.dnode].
#'
#' @param nlev (integer vector) of cardinality of the outcome variable and the
#'  parent variables (the dimensions of the CPT). First element correspond to the outcome,
#'  the rest to its parents.
#' @param alpha (numeric vector)
#' @param method (character) Which method to use. The direct method (default) is
#' to assume `alpha` specifies the Dirichlet hyperparameters associated with each
#' outcome. Specifically, a symmetric Dirichlet distribution if `alpha` is a constant,
#' a single Dirichlet distribution if `alpha` is a vector of length `nlev[1]` or
#' a (possibly) unique Dirichlet distribution for each parent level if `alpha` is
#' of length `prod(nlev)`. If method equals `CM`, each conditional distribution is
#' drawn by perturbing a vector of means as described in XX.  `alpha` is then taken
#' to be the imaginary sample size / precision parameter controlling the variance of
#' the Dirichlet distribution.
#' @param scope (character vector) names of the variables. Defaults to `names(nlev)`.
#'
#' @return an array with dimension `nlev` representing a CPT.
#' @keywords internal
#'
#' @examples
#'
#' # no parents
#' cpt <- rand_cpt(nlev = 2, scope = "Y")
#' cpt
#' dimnames(cpt)
#'
#' # two parents
#' cpt <- rand_cpt(2:4, scope = c("Y", "X", "Z"))
#' cpt
#'
#' # with local structure
#' \dontrun{
#' cpt <- rand_cpt(nlev, local_struct = "tree", prob = 1, maxdepth = .5)
#' }

rand_cpt <- function(nlev,
                     alpha = 1,
                     method = c("direct", "cm"),
                     scope = character(),
                     ...) {

  method <- match.arg(method)

  r <- nlev[1]
  q <- prod(nlev[-1])

  if (method == "cm") {
    tmp <- 1/seq_len(r)
    mu <- tmp/sum(tmp)
    p <- matrix(NA, r, q)
    for (qq in seq_len(q)){
      p[, qq] <- rDirichlet(1, alpha*mu)
      mu <- c(mu[r], mu[-r])
    }
  } else {
    # draw one Dirichlet vector for each parent config
    if (length(alpha) == 1) {
      p <- t(rDirichlet(q, rep(alpha, r), r))
    } else if (length(alpha) == r) {
      p <- t(rDirichlet(q, alpha, r))
    } else if (length(alpha) == q*r) {
      p <- vapply(split(alpha, rep(seq_len(q), each = r)),
                  bida:::rDirichlet, n = 1, k = r,
                  numeric(r))
    } else {
      stop("The hyperparamter alpha must be either length 1, nlev[1] or prod(nlev).")
    }
  }


  dim(p) <- nlev
  if (length(scope) > 0) {
    dimnames <- lapply(nlev-1, seq.int, from = 0)
    names(dimnames) <- scope
    dimnames(p) <- dimnames
  }
  return(p)
}

#' @param local_struct (character) name of algorithm to form a partitioning /
#'  produce parameter restrictions.
#'  Defaults to `"none"`, which returns a CPT with no parameter restrictions.
#' @param ... additional arguments sendt to [rand_partition].
# rand_cpt_lstruct <- function() {
#   is_local_struct <- length(nlev) > 2 && local_struct != "none"
#
#   if (is_local_struct) {
#     args <- list(...)
#     args$nlev <- nlev[-1]
#     args$method <- local_struct
#     print(args)
#     tmp <- do.call(rand_partition, args)
#     print(tmp)
#     q <- length(unique(tmp$parts))
#   } else {
#     q <- prod(nlev[-1])
#   }
#
#   if (is_local_struct) {
#     p <- t(rDirichlet(length(tmp$partition), alpha, r))[, tmp$parts]
#     attr(p, "parts") <- tmp$parts
#   }
#
# }

