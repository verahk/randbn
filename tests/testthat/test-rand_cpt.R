test_that("default version return correct dim and dimnames", {
  nlev <- c("Y" = 2, "X" = 3, "Z" = 4)
  levels <- lapply(nlev-1, function(x) as.character(seq.int(x, from = 0)))
  
  cpt <- rand_cpt(nlev, scope = names(nlev))
  expect_equal(dim(cpt), nlev)
  expect_equal(dimnames(cpt), levels)
})

test_that("each conditional distirbution sum to one", {

  dims <- 2
  expect_true(sum(rand_cpt(dims)) == 1)
  expect_true(sum(rand_cpt_cm(dims)) == 1)
  
  dims <- 2:5
  expect_true(all(colSums(rand_cpt(dims)) == 1))
  
  dims <- 5:2
  expect_true(all(colSums(rand_cpt_cm(dims))-1 < 10**-15))
})
