test_that("masked_array indexing works on vectors", {
  x = MaskedArray(1:10, dynamic_mask = FALSE)
  y = x[2:8][2:4]
  expect_s4_class(y, "MaskedArray")
  # Indexing should change the original vector, and the mask
  expect_false(identical(x@value, y@value))
  expect_false(y@dynamic_mask)
  expect_equal(y@masks[[1]], 3:5)
  expect_equal(y@value, 3:5)
  expect_equal(apply_mask(y), 3:5)
})

test_that("masked_array indexing works on matrices", {
  x = MaskedArray(matrix(1:16, nrow=4), dynamic_mask = FALSE)
  y = x[2:3, 2:3]
  expect_s4_class(y, "MaskedArray")
  expect_identical(x@value, y@value)
  expect_equal(y@masks[[1]], 2:3)
  expect_equal(y@masks[[2]], 2:3)
  # After applying the mask, we should only have the middle 2x2 matrix
  expect_equal(apply_mask(y), matrix(c(6L, 7L, 10L, 11L), nrow=2))
})

test_that("masked_array indexing works on data frames", {
  x = MaskedArray(iris, dynamic_mask = FALSE)
  idx_1 = 2:5
  idx_2 = c("Petal.Length", "Sepal.Length")
  y = x[idx_1, idx_2]

  expect_s4_class(y, "MaskedArray")
  expect_identical(x@value, y@value)
  expect_equal(y@masks[[1]], idx_1)
  expect_equal(y@masks[[2]], c(3, 1))
  expect_equal(apply_mask(y), iris[idx_1, idx_2])
})
