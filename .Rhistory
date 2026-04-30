install.packages(c("usethis", "devtools", "testthat", "roxygen2", "covr", "lintr"))

usethis::create_package("triangdist")
usethis::use_git()
usethis::use_github()
usethis::use_testthat()
usethis::use_r("triang_dist")

# Density function

#' @param x Vector of values
#' @param min Lower limit
#' @param max Upper limit
#' @param mode Mode (peak)
#'
#' @return Density values
#' @export

dtriang <- function(x, min, max, mode) {
  if (min >= max) stop("Min debe ser < max")
  if (mode < min || mode > max) stop("Mode debe encontrarse dentro de [min, max]")
  h <- 2 / (max - min)
  res <- ifelse(
    x < min | x > max, 0,
    ifelse(
      x <= mode,
      h * (x - min) / (mode - min),
      h * (max - x) / (max - mode)
    )
  )
  return(res)
}

# Distribution function:

#' @param q Quantiles
#' @param min Lower limit
#' @param max Upper limit
#' @param mode Mode
#'
#' @return CDF values
#' @export

ptriang <- function(q, min, max, mode) {
  if (min >= max) stop("Min debe ser < max")
  if (mode < min || mode > max) stop("Mode debe encontrarse dentro de [min, max]")
  res <- ifelse(
    q <= min, 0,
    ifelse(
      q >= max, 1,
      ifelse(
        q <= mode,
        (q - min)^2 / ((max - min) * (mode - min)),
        1 - (max - q)^2 / ((max - min) * (max - mode))
      )
    )
    return(res)
}

# Quantile function:

#' @param p Probabilities
#' @param min Lower limit
#' @param max Upper limit
#' @param mode Mode
#'
#' @return Quantiles
#' @export

qtriang <- function(p, min, max, mode) {
  if (min >= max) stop("Min debe ser > max")
  if (mode < min || mode > max) stop("Mode debe encontrarse dentro de [min, max]")
  if (any(p < 0 | p > 1)) stop("p debe encontrarse dentro de [0,1]")
  Fc <- (mode - min) / (max - min)
  res <- ifelse(
    p <= Fc,
    min + sqrt(p * (max - min) * (mode - min)),
    max - sqrt((1 - p) * (max - min) * (max - mode))
  )
  return(res)
}

# Random generation:

#' @param n Number of samples
#' @param min Lower limit
#' @param max Upper limit
#' @param mode Mode
#'
#' @return Random values
#' @export

rtriang <- function(n, min, max, mode) {
  u <- runif(n)
  return(qtriang(u, min, max, mode))
}

usethis::use_testthat()

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

devtools::document()

remotes::install_github("viciipa/triangdist")

covr::report()
lintr::lint_package()
devtools::check()
