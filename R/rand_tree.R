

#' Grow a random decision tree 
#'
#' @param dims 
#' @param p 
#' @param branch 
#' @param mindepth 
#' @param maxdepth 
#'
#' @return
#' @export
#'
#' @examples
#' dims <- 2:4
#' names(dims) <- letters[seq_along(dims)]
#' tree <- rand_tree(dims, .5)
#' tree
#'  
#' tree <- rand_tree(dims, 1) 
#' summary(tree)$nparts == prod(dims)
#' 
#' tree <- rand_tree(dims, 0, mindepth = 0) 
#' summary.tree(tree)$nparts == 1
#' 
#' # mindepth and maxdepth overrules `p`
#' tree <- rand_tree(dims, 0, mindepth = length(dims)) 
#' summary.tree(tree)$nparts == prod(dims)
#' tree <- rand_tree(dims, 1, maxdepth = 0) 
#' summary.tree(tree)$nparts == 1
#'  
#' # predict   
#' newdata <- expand.grid(lapply(dims-1, seq.int, from = 0))
#' predict(tree, newdata)
#' 
#' # regular
#' tree <- rand_tree(dims, 0, mindepth = 1) 
#' make_regular(tree, dims)
#' 
rand_tree <- function(dims, p, branch = "", mindepth = 1, maxdepth = length(dims)) {
  
  depth <- stringr::str_count(branch, "\\|")
  if ((length(dims) == 0) || depth >= maxdepth || (depth >= mindepth && runif(1) > p)) {
    out <- sprintf("%s--\n", branch)
  } else {
    var <- sample.int(length(dims), 1)
    split  <- sprintf("%s-+ %s:\n", branch, names(dims)[[var]])
    new_branch <- paste0(branch, " | ")
    out <- c(split, unlist(lapply(seq_len(dims[[var]]), 
                                  function(x) rand_tree(dims[-var], p, new_branch, mindepth, maxdepth))))
  }
  structure(out, class = c("tree"))
}

print.tree <- function(tree, prefix = "") {
 cat(tree)
}

summary.tree <- function(tree) {
  
  leaves <- grepl("--\n$", tree)
  splits <- grepl("-\\+", tree)
  
  list(nparts = sum(leaves),
       predictors = unique(stringr::str_match(tree[splits], "-\\+ (.*):")[, 2]))
}

predict.tree <- function(tree, newdata) {
  if (!length(dim(newdata)) == 2) stop("newdata must be a matrix or data.frame")
  if (is.null(colnames(newdata))) stop("newdata must have column names")
  
  if (length(tree) == 1){
    return(rep(1L, nrow(newdata)))
  } else {
    rules <- rules_from_tree(tree)
    newdata <- as.data.frame(newdata)
    parts   <- vector("integer", nrow(newdata))
    for (part in seq_along(rules)) {
      indx <- with(newdata, eval(parse(text = rules[[part]])))
      parts[indx] <- part
    }
    return(parts)
  }
}

add_split <- function(tree, dims, pos) {
  leaf <- tree[pos]
  stopifnot(grepl("--\n$", tree[pos]))
  new_branch <- rand_tree(dims, 1, gsub("--\n$", "", leaf), maxdepth = Inf)
  append(tree[-pos], new_branch, pos-1)
}

make_regular <- function(tree, dims) {
  predictors <- summary.tree(tree)$predictors
  if (length(predictors) < length(dims)) {
    missing_variables <- setdiff(names(dims), predictors)
    for (v in missing_variables) {
      pos  <- sample(grep("--\n$", tree), 1)   # draw a random leaf
      tree <- add_split(tree, dims[v], pos) # grow a tree-.  
    }
  }
  return(tree)
}
rules_from_tree <- function(tree, rule = "") {
  # recurcive function that returns the rules defining each leaf of the tree
  if (grepl("^\\-\\+", tree[1])) { # check that root is not a leaf
    root <- tree[1]
    var <- substring(root, 4, nchar(root)-2)
    
    # collect subtrees
    tmp <- gsub("^ \\| ", "", tree[-1])         # remove pre-fix
    pos <- grep("^-+|^--", tmp)                 # positions of next nodes or leaves
    subtrees <- split(tmp, rep.int(seq_along(pos), diff(c(pos, length(tmp)+1))))
    
    # update rules
    new_rules <- lapply(seq_along(pos)-1,
                        function(x) paste0(rule, " & ", var, "==", x))
    
    # add rules of each subtree
    unlist(lapply(seq_along(subtrees),
                  function(b) rules_from_tree(subtrees[[b]], new_rules[[b]])),
           recursive = FALSE)
    
  } else {
    #list(parse(text = gsub("^ & ", "", rule)))
    gsub("^ & ", "", rule) # remove first "&" from string
  }
}




