

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
#' cat(tree, sep = "")
#' 
#' configs <- expand.grid(lapply(dims-1, seq.int, from = 0))
#' parts   <- partition_from_tree(tree, newdata)
#' cbind(configs, parts)
#'  
#' # full depth
#' tree <- rand_tree(dims, 1) 
#' cat(tree, sep = "")
#' 
#' # no splits
#' tree <- rand_tree(dims, 0) 
#' cat(tree, sep = "")
#' 
#' # stump / one split
#' tree <- rand_tree(dims, 0, mindepth = 1)
#' cat(tree, sep = "")
#' 
#' # mindepth and maxdepth overrules `p`
#' tree <- rand_tree(dims, 0, mindepth = length(dims)) 
#' cat(tree, sep = "")
#' tree <- rand_tree(dims, 1, maxdepth = 0) 
#' cat(tree, sep = "")
#'  
#' 
#' # regular overrules `mindepth`
#' tree <- rand_tree(dims, 1, maxdepth = 0, regular = TRUE) 
#' cat(tree, sep = "")
#' 
#' 
rand_tree <- function(dims, p, mindepth = 0, maxdepth = Inf, regular = FALSE) {
  
  tree <- grow_tree(dims, p, branch = "", mindepth = mindepth, maxdepth = maxdepth)
  
  if (regular) {
    tree <- make_tree_regular(tree, dims)
  }
  
  tree
}

# grow a random decision tree
grow_tree <- function(dims, p, branch, mindepth, maxdepth) {
  depth <- stringr::str_count(branch, "\\|")
  if ((length(dims) == 0) || depth >= maxdepth || (depth >= mindepth && runif(1) > p)) {
    out <- sprintf("%s--\n", branch)
  } else {
    var <- sample.int(length(dims), 1)
    split  <- sprintf("%s-+ %s:\n", branch, names(dims)[[var]])
    new_branch <- paste0(branch, " | ")
    out <- c(split, unlist(lapply(seq_len(dims[[var]]), 
                                  function(x) grow_tree(dims[-var], p, new_branch, mindepth, maxdepth))))
  }
  return(out)
}

#' @rdname rand_tree
#' @export
make_tree_regular <- function(tree, dims) {
 splits <- grepl("-\\+", tree)
 predictors <- unique(stringr::str_match(tree[splits], "-\\+ (.*):")[, 2])
  if (length(predictors) < length(dims)) {
    missing_variables <- setdiff(names(dims), predictors)
    for (v in missing_variables) {
      pos  <- sample( grep("--\n$", tree)) # sample a leaf
      leaf <- tree[pos]
      new_branch <- grow_tree(dims[v], 1, gsub("--\n$", "", leaf), mindepth = 1, maxdepth = Inf)
      tree <- append(tree[-pos], new_branch, pos-1)
    }
  }
  return(tree)
}



#' @rdname rand_tree
#' @export
partition_from_tree <- function(tree, newdata) {
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




