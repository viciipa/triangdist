test_that("dtriang works", {
  expect_equal(dtriang(0, 0, 1, 0.5), 0)
  expect_true(all(dtriang(c(0.2,0.5), 0,1,0.5) >= 0))
})

test_that("ptriang works", {
  expect_equal(ptriang(0, 0, 1, 0.5), 0)
  expect_equal(ptriang(1, 0, 1, 0.5), 1)
})

test_that("qtriang works", {
  expect_true(qtriang(0.5, 0, 1, 0.5) > 0)
})

test_that("rtriang works", {
  x <- rtriang(1000, 0, 1, 0.5)
  expect_equal(length(x), 1000)
})

test_that("errors work", {
  expect_error(dtriang(1, 1, 0, 0.5))
  expect_error(qtriang(-0.1, 0, 1, 0.5))
})