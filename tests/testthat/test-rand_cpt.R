test_that("default version return correct dim and dimnames", {
  nlev <- c("Y" = 2, "X" = 3, "Z" = 4)
  cpt <- rand_cpt(nlev, scope = names(nlev))
  expect_equal(dim(cpt), nlev)
  expect_equal(names(dimnames(cpt)), names(nlev))
})

test_that("cm version return correct dim and dimnames", {
  nlev <- c("Y" = 2, "X" = 3, "Z" = 4)
  cpt <- rand_cpt(nlev, scope = names(nlev), method = "cm", alpha = 10)
  expect_equal(dim(cpt), nlev)
  expect_equal(names(dimnames(cpt)), names(nlev))
})
