
<!-- README.md is generated from README.Rmd. Please edit that file -->

# randbn

<!-- badges: start -->

<!-- badges: end -->

This repository contains a small R-package with function I use
frequently to draw random Bayesian networks. It is based on routines
provided in the `pcalg` and `bnlearn` package.

## Installation

You can install the development version of randbn from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("verahk/randbn")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(randbn)

r <- 007           # seed number
n <- 3             # number of nodes
d <- 1             # expected number of neighbours
nlev <- rep(2, n)  # cardinality of each variable

set.seed(r)
bn <- rand_bn(n, d, "cat", nlev = nlev, alpha = 1)
bn
#> 
#>   Bayesian network parameters
#> 
#>   Parameters of node X1 (multinomial distribution)
#> 
#> Conditional probability table:
#>  X1
#>         0         1 
#> 0.9720625 0.0279375 
#> 
#>   Parameters of node X2 (multinomial distribution)
#> 
#> Conditional probability table:
#>  
#>    X3
#> X2          0         1
#>   0 0.1658555 0.4591037
#>   1 0.8341445 0.5408963
#> 
#>   Parameters of node X3 (multinomial distribution)
#> 
#> Conditional probability table:
#>  X3
#>         0         1 
#> 0.1717481 0.8282519
```

Replicate with the distinct functions:

``` r
set.seed(r)
dag  <- rand_dag(n, d) # draw an adjacency matrix 
dist <- rand_dist(dag, "cat", nlev = nlev, alpha = 1)  # draw set of local dist
bn1  <- custom_bn(dag, dist, use_bnlearn = FALSE)       # construct bn.fit() object
all.equal(bn1, bn)
#> [1] TRUE
```

## Example 2: Add intervention node to bn

``` r

dag <- matrix(0, 2, 2)
dag[1, 2] <- 1
dist <- rand_dist(dag, "cat", nlev = rep(2, ncol(dag)), alpha = 1)
bn <- custom_bn(dag, dist)

# generate a new intervention node to produce an augmented network
node <- new_node("I", children = "X2", prob = array(1:0, 2, list(I = c("off", "on"))))
ls.str(node)
#> children :  chr "X2"
#> node :  chr "I"
#> parents :  chr(0) 
#> prob :  int [1:2(1d)] 1 0

# imperfect intervention: 
# - draw local intervention distributions from a uniform Dirichlet
bn_augmented <- add_node_to_bn(bn, node)
bn_augmented$X2
#> 
#>   Parameters of node X2 (multinomial distribution)
#> 
#> Conditional probability table:
#>  
#> , , I = off
#> 
#>    X1
#> X2          0          1
#>   0 0.7728119 0.09630154
#>   1 0.2271881 0.90369846
#> 
#> , , I = on
#> 
#>    X1
#> X2          0          1
#>   0 0.4534478 0.08470071
#>   1 0.5465522 0.91529929

# almost-perfect intervention: 
# - draw local intervention distributions from a the same, skewed Dirichlet
mu <- c(.9, .1)
ess <- 1000
bn_augmented <- add_node_to_bn(bn, node, alpha = mu*ess)
bn_augmented$X2
#> 
#>   Parameters of node X2 (multinomial distribution)
#> 
#> Conditional probability table:
#>  
#> , , I = off
#> 
#>    X1
#> X2          0          1
#>   0 0.7728119 0.09630154
#>   1 0.2271881 0.90369846
#> 
#> , , I = on
#> 
#>    X1
#> X2          0          1
#>   0 0.8968614 0.90116033
#>   1 0.1031386 0.09883967

# perfect intervention: deterministic local intervention distributions 
generate_local_dist <- function(dims) {
  new_rows <- array(0,  dims)
  for (ss in 1:(dims[3]-1)) new_rows[ss,,ss] <- 1
  return(new_rows)
}
bn_augmented <- add_node_to_bn(bn, node, generate_local_dist = generate_local_dist)
bn_augmented$X2
#> 
#>   Parameters of node X2 (multinomial distribution)
#> 
#> Conditional probability table:
#>  
#> , , I = off
#> 
#>    X1
#> X2          0          1
#>   0 0.7728119 0.09630154
#>   1 0.2271881 0.90369846
#> 
#> , , I = on
#> 
#>    X1
#> X2  0 1
#>   0 1 1
#>   1 0 0
```
