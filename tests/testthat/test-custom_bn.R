test_that("custom_bn return the same as bnlearn::custom.bn", {
  dag <- rand_dag(3, 2)
  dist <- rand_dist(dag, type = "cat", nlev = c(2, 2, 2))
  expect_equal(custom_bn(dag, dist, use_bnlearn = TRUE),
               custom_bn(dag, dist, use_bnlearn = FALSE))
})

test_that("throws error if CPTs has no dimnames", {
  dist <- list(rand_cpt(2, 1),
               rand_cpt(2, 1),
               rand_cpt(c(2, 2, 2), alpha = 1))
  expect_error(custom_bn(dag, dist, FALSE))
  
})

