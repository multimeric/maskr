test_that("tail works on masked vectors", {
  x = MaskedArray(1:10)
  y = tail(x, n=5)
  expect_s4_class(y, "MaskedArray")
  expect_identical(x@value, y@value)
  expect_equal(apply_mask(y), 6:10)
})

test_that("masked_array indexing works on vectors", {
  x = MaskedArray(1:10)
  y = x[2:8][2:4]
  expect_s4_class(y, "MaskedArray")
  expect_identical(x@value, y@value)
  expect_equal(y@masks[[1]], 3:5)
  expect_equal(apply_mask(y), 3:5)
  expect_equal(as.double(y), 3:5)
  expect_equal(as.vector(y), 3:5)
  expect_equal(as.numeric(y), 3:5)
  expect_equal(length(y), 3)
})

test_that("masked_array indexing works on matrices", {
  x = MaskedArray(matrix(1:16, nrow=4))
  y = x[2:3, 2:3]
  expect_s4_class(y, "MaskedArray")
  expect_identical(x@value, y@value)
  expect_equal(y@masks[[1]], 2:3)
  expect_equal(y@masks[[2]], 2:3)
  # After applying the mask, we should only have the middle 2x2 matrix
  post_mask = matrix(c(6L, 7L, 10L, 11L), nrow=2)
  expect_equal(as.matrix(y), post_mask)
  expect_equal(apply_mask(y), post_mask)
  # Some methods should consider the post-mask object
  expect_equal(dim(y), c(2, 2))
})

test_that("masked_array indexing works on data frames", {
  x = MaskedArray(iris)
  idx_1 = 2:5
  idx_2 = c("Petal.Length", "Sepal.Length")
  y = x[idx_1, idx_2]

  expect_s4_class(y, "MaskedArray")
  expect_identical(x@value, y@value)
  expect_equal(y@masks[[1]], idx_1)
  expect_equal(y@masks[[2]], c(3, 1))
  expect_equal(apply_mask(y), iris[idx_1, idx_2])
})
