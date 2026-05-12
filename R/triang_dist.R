#' @import stats
NULL

#' Triangular density
#'
#' @param x Vector of values
#' @param min Lower limit
#' @param max Upper limit
#' @param mode Mode (peak)
#' @return Density values
#' @export
#' @examples
#' dtriang(0.5, 0, 1, 0.5)
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

#' Triangular cumulative distribution
#'
#' @param q Quantiles
#' @param min Lower limit
#' @param max Upper limit
#' @param mode Mode
#' @return CDF values
#' @export
#' @examples
#' ptriang(0.5, 0, 1, 0.5)
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
  )
  return(res)
}

#' Triangular quantile function
#'
#' @param p Probabilities
#' @param min Lower limit
#' @param max Upper limit
#' @param mode Mode
#' @return Quantiles
#' @export
#' @examples
#' qtriang(0.5, 0, 1, 0.5)
qtriang <- function(p, min, max, mode) {
  if (min >= max) stop("Min debe ser < max")
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

#' Random generation from triangular distribution
#'
#' @param n Number of samples
#' @param min Lower limit
#' @param max Upper limit
#' @param mode Mode
#' @return Random values
#' @export
#' @examples
#' rtriang(10, 0, 1, 0.5)
rtriang <- function(n, min, max, mode) {
  u <- runif(n)
  return(qtriang(u, min, max, mode))
}