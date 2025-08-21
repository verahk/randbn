
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
n <- 10            # number of nodes
d <- 3             # expected number of neighbours
nlev <- rep(2, n)  # cardinality of each variable

set.seed(r)
rand_bn(n, d, "cat", nlev = nlev, alpha = 1)
#> 
#>   Bayesian network parameters
#> 
#>   Parameters of node X1 (multinomial distribution)
#> 
#> Conditional probability table:
#>  
#>    X3
#> X1          0         1
#>   0 0.1872283 0.3912495
#>   1 0.8127717 0.6087505
#> 
#>   Parameters of node X2 (multinomial distribution)
#> 
#> Conditional probability table:
#>  
#> , , X6 = 0, X9 = 0
#> 
#>    X3
#> X2          0         1
#>   0 0.2739012 0.1919177
#>   1 0.7260988 0.8080823
#> 
#> , , X6 = 1, X9 = 0
#> 
#>    X3
#> X2          0         1
#>   0 0.5043918 0.7638404
#>   1 0.4956082 0.2361596
#> 
#> , , X6 = 0, X9 = 1
#> 
#>    X3
#> X2          0         1
#>   0 0.6936689 0.5440542
#>   1 0.3063311 0.4559458
#> 
#> , , X6 = 1, X9 = 1
#> 
#>    X3
#> X2          0         1
#>   0 0.6590872 0.4687284
#>   1 0.3409128 0.5312716
#> 
#> 
#>   Parameters of node X3 (multinomial distribution)
#> 
#> Conditional probability table:
#>  X3
#>         0         1 
#> 0.4818055 0.5181945 
#> 
#>   Parameters of node X4 (multinomial distribution)
#> 
#> Conditional probability table:
#>  
#>    X8
#> X4          0         1
#>   0 0.3370636 0.4245263
#>   1 0.6629364 0.5754737
#> 
#>   Parameters of node X5 (multinomial distribution)
#> 
#> Conditional probability table:
#>  
#>    X1
#> X5          0         1
#>   0 0.2870151 0.6011915
#>   1 0.7129849 0.3988085
#> 
#>   Parameters of node X6 (multinomial distribution)
#> 
#> Conditional probability table:
#>  X6
#>         0         1 
#> 0.8407423 0.1592577 
#> 
#>   Parameters of node X7 (multinomial distribution)
#> 
#> Conditional probability table:
#>  
#>    X3
#> X7          0         1
#>   0 0.6208370 0.1345516
#>   1 0.3791630 0.8654484
#> 
#>   Parameters of node X8 (multinomial distribution)
#> 
#> Conditional probability table:
#>  
#>    X6
#> X8          0         1
#>   0 0.5677224 0.4434263
#>   1 0.4322776 0.5565737
#> 
#>   Parameters of node X9 (multinomial distribution)
#> 
#> Conditional probability table:
#>  
#> , , X3 = 0, X8 = 0
#> 
#>    X1
#> X9           0          1
#>   0 0.43797542 0.62361723
#>   1 0.56202458 0.37638277
#> 
#> , , X3 = 1, X8 = 0
#> 
#>    X1
#> X9           0          1
#>   0 0.93265334 0.88849258
#>   1 0.06734666 0.11150742
#> 
#> , , X3 = 0, X8 = 1
#> 
#>    X1
#> X9           0          1
#>   0 0.87854056 0.24217695
#>   1 0.12145944 0.75782305
#> 
#> , , X3 = 1, X8 = 1
#> 
#>    X1
#> X9           0          1
#>   0 0.74145380 0.38765631
#>   1 0.25854620 0.61234369
#> 
#> 
#>   Parameters of node X10 (multinomial distribution)
#> 
#> Conditional probability table:
#>  
#> , , X3 = 0, X4 = 0, X5 = 0, X8 = 0
#> 
#>    X2
#> X10           0           1
#>   0 0.078951739 0.094835550
#>   1 0.921048261 0.905164450
#> 
#> , , X3 = 1, X4 = 0, X5 = 0, X8 = 0
#> 
#>    X2
#> X10           0           1
#>   0 0.762142731 0.347894026
#>   1 0.237857269 0.652105974
#> 
#> , , X3 = 0, X4 = 1, X5 = 0, X8 = 0
#> 
#>    X2
#> X10           0           1
#>   0 0.416766709 0.344016231
#>   1 0.583233291 0.655983769
#> 
#> , , X3 = 1, X4 = 1, X5 = 0, X8 = 0
#> 
#>    X2
#> X10           0           1
#>   0 0.008410923 0.911574991
#>   1 0.991589077 0.088425009
#> 
#> , , X3 = 0, X4 = 0, X5 = 1, X8 = 0
#> 
#>    X2
#> X10           0           1
#>   0 0.182205419 0.722803449
#>   1 0.817794581 0.277196551
#> 
#> , , X3 = 1, X4 = 0, X5 = 1, X8 = 0
#> 
#>    X2
#> X10           0           1
#>   0 0.571963331 0.540036414
#>   1 0.428036669 0.459963586
#> 
#> , , X3 = 0, X4 = 1, X5 = 1, X8 = 0
#> 
#>    X2
#> X10           0           1
#>   0 0.354947415 0.824091838
#>   1 0.645052585 0.175908162
#> 
#> , , X3 = 1, X4 = 1, X5 = 1, X8 = 0
#> 
#>    X2
#> X10           0           1
#>   0 0.186136761 0.396374585
#>   1 0.813863239 0.603625415
#> 
#> , , X3 = 0, X4 = 0, X5 = 0, X8 = 1
#> 
#>    X2
#> X10           0           1
#>   0 0.486214877 0.496940902
#>   1 0.513785123 0.503059098
#> 
#> , , X3 = 1, X4 = 0, X5 = 0, X8 = 1
#> 
#>    X2
#> X10           0           1
#>   0 0.387040419 0.643247554
#>   1 0.612959581 0.356752446
#> 
#> , , X3 = 0, X4 = 1, X5 = 0, X8 = 1
#> 
#>    X2
#> X10           0           1
#>   0 0.343830247 0.956104083
#>   1 0.656169753 0.043895917
#> 
#> , , X3 = 1, X4 = 1, X5 = 0, X8 = 1
#> 
#>    X2
#> X10           0           1
#>   0 0.042711742 0.765387396
#>   1 0.957288258 0.234612604
#> 
#> , , X3 = 0, X4 = 0, X5 = 1, X8 = 1
#> 
#>    X2
#> X10           0           1
#>   0 0.203770043 0.684754565
#>   1 0.796229957 0.315245435
#> 
#> , , X3 = 1, X4 = 0, X5 = 1, X8 = 1
#> 
#>    X2
#> X10           0           1
#>   0 0.396167753 0.058455937
#>   1 0.603832247 0.941544063
#> 
#> , , X3 = 0, X4 = 1, X5 = 1, X8 = 1
#> 
#>    X2
#> X10           0           1
#>   0 0.736253970 0.624852455
#>   1 0.263746030 0.375147545
#> 
#> , , X3 = 1, X4 = 1, X5 = 1, X8 = 1
#> 
#>    X2
#> X10           0           1
#>   0 0.629168641 0.565918159
#>   1 0.370831359 0.434081841
```

Replicate with the distinct functions:

``` r
set.seed(r)
dag  <- rand_dag(10, 3) # draw an adjacency matrix 
dist <- rand_dist(dag, "cat", nlev = nlev, alpha = 1)  # draw set of local dist
bn   <- custom_bn(dag, dist, use_bnlearn = FALSE)       # construct bn.fit() object
```
