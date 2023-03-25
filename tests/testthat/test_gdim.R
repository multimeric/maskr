test_that("gdim works on all types", {
  expect_equal(gdim(1:5), 5)
  expect_equal(gdim(matrix(1:4, nrow=2)), c(2, 2))
  expect_equal(gdim(data.frame(x=1:4, y=1:4)), c(4, 2))
})
