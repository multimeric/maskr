test_that("map_indices works", {
  # Suppose the original array looks like this:
  # x = MaskedArray(letters[1:6], mask=c(1, 2, 5, 6))
  # indices 3 and 4 are hidden. So x[c(2, 3)] should return c(2, 5)
  expect_equal(
    map_indices(index=c(2, 3), mask=c(1, 2, 5, 6), axis_len=6),
    c(2, 5)
  )
})
