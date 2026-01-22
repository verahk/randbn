test_that("splitting probability workds", {
  dims <- 2:4
  names(dims) <- letters[seq_along(dims)]
  
  # splitting prob of 1 returns full-grown tree
  tree <- rand_tree(dims, 1)
  expect_equal(summary.tree(tree)$nparts, prod(dims))
  expect_s3_class(tree, "tree")
  
  # splitting prob of 0 returns empty tree (when mindepth allows for it !)
  tree <- rand_tree(dims, 0, mindepth = 0)
  expect_equal(summary.tree(tree)$nparts, 1)
  expect_s3_class(tree, "tree")
  
  # mindepth and maxdepth overrules `p`
  tree <- rand_tree(dims, 0, mindepth = length(dims))
  expect_equal(summary.tree(tree)$nparts, prod(dims))
  tree <- rand_tree(dims, 1, maxdepth = 0)
  expect_equal(summary.tree(tree)$nparts, 1)
  expect_s3_class(tree, "tree")
})

test_that("make_regular() works", {
  dims <- 2:4
  names(dims) <- letters[seq_along(dims)]
  tree <- rand_tree(dims, 0, mindepth = 0)
  new_tree <- make_regular(tree, dims[1])
  expect_equal(new_tree, c("-+ a:\n", " | --\n", " | --\n"))
})


