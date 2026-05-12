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

# Validación de errores (mensajes reales)
test_that("errors work", {
  expect_error(dtriang(1, 1, 0, 0.5), "Min debe ser < max")
  expect_error(qtriang(-0.1, 0, 1, 0.5), "p debe encontrarse")
  expect_error(dtriang(0.5, 0, 1, 2), "Mode debe encontrarse")
  expect_error(ptriang(0.5, 0, 1, -0.5), "Mode debe encontrarse")
  expect_error(qtriang(1.5, 0, 1, 0.5), "p debe encontrarse")
  # rtriang con n negativo: runif da error "invalid arguments"
  expect_error(rtriang(-5, 0, 1, 0.5), "invalid arguments")
})

# Cobertura rama x > mode en dtriang
test_that("dtriang - rama x > mode", {
  expect_equal(dtriang(0.8, 0, 1, 0.5), 2 * (1 - 0.8) / (1 - 0.5))
})

# Cobertura rama q > mode en ptriang
test_that("ptriang - rama q > mode", {
  expect_equal(ptriang(0.8, 0, 1, 0.5), 1 - (1 - 0.8)^2 / ((1 - 0) * (1 - 0.5)))
})

# Cobertura q == mode en ptriang
test_that("ptriang - q == mode", {
  expect_equal(ptriang(0.5, 0, 1, 0.5), 0.5)
})

# Cobertura de p > Fc en qtriang
test_that("qtriang - p > Fc", {
  expect_equal(qtriang(0.8, 0, 1, 0.5), 1 - sqrt((1 - 0.8) * (1 - 0) * (1 - 0.5)))
})

# Consistencia entre qtriang y ptriang
test_that("Consistencia qtriang-ptriang", {
  p <- 0.3
  q <- qtriang(p, 0, 1, 0.5)
  expect_equal(ptriang(q, 0, 1, 0.5), p, tolerance = 1e-6)
})

# Vectorización
test_that("Vectorización", {
  x_vec <- c(0.2, 0.5, 0.8)
  expect_equal(length(dtriang(x_vec, 0, 1, 0.5)), 3)
  expect_equal(length(ptriang(x_vec, 0, 1, 0.5)), 3)
  expect_equal(length(qtriang(c(0.2,0.5,0.8), 0, 1, 0.5)), 3)
  expect_equal(length(rtriang(3, 0, 1, 0.5)), 3)
})

# Modo muy cercano a min (pero no exacto, evita NaN)
test_that("mode próximo a min (sin división por cero)", {
  mode <- 1e-8
  x <- mode / 2
  expect_true(dtriang(x, 0, 1, mode) > 0)
  expect_true(ptriang(x, 0, 1, mode) >= 0)
  expect_true(ptriang(mode * 2, 0, 1, mode) <= 1)
})

# Modo muy cercano a max
test_that("mode próximo a max", {
  mode <- 1 - 1e-8
  x <- mode + 1e-9
  expect_true(dtriang(x, 0, 1, mode) > 0)
  expect_true(ptriang(x, 0, 1, mode) <= 1)
})

# Cobertura de stop("Min debe ser < max") en ptriang y rtriang
test_that("ptriang y rtriang detectan min >= max", {
  expect_error(ptriang(0.5, 1, 0, 0.5), "Min debe ser < max")
  expect_error(rtriang(1, 1, 0, 0.5), "Min debe ser < max")
})

# Cobertura de stop("Mode debe encontrarse...") en ptriang y rtriang
test_that("ptriang y rtriang detectan mode fuera de rango", {
  expect_error(ptriang(0.5, 0, 1, 2), "Mode debe encontrarse")
  expect_error(rtriang(1, 0, 1, -0.5), "Mode debe encontrarse")
})