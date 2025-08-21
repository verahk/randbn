
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
rand_bn(n, d, "cat", nlev = nlev, alpha = 1)
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
<<<<<<< HEAD

=======
>>>>>>> a074fce (...change example in README)
dag  <- rand_dag(n, d) # draw an adjacency matrix 
dist <- rand_dist(dag, "cat", nlev = nlev, alpha = 1)  # draw set of local dist
bn   <- custom_bn(dag, dist, use_bnlearn = FALSE)       # construct bn.fit() object
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
