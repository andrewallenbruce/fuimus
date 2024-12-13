test_that("summary_stats() works", {

  set.seed(1234)

  a <- dplyr::tibble(
    provider = sample(c("A", "B", "C"), size = 50, replace = TRUE),
    city     = sample(c("ATL", "NYC"), size = 50, replace = TRUE),
    charges  = sample(1000:2000, size = 50),
    payment  = sample(1000:2000, size = 50))

  sm <- summary_stats(a,
                condition    = city == "ATL",
                group_vars   = provider,
                summary_vars = c(charges, payment),
                arr          = n)

  b <- dplyr::tibble(
    provider       = c("B", "A", "C"),
    charges_median = c(1433, 1540, 1570.5),
    charges_mean   = c(1403.083, 1533.143, 1529.333),
    charges_sd     = c(233.785, 277.181, 348.370),
    payment_median = c(1763, 1443, 1433),
    payment_mean   = c(1628.083, 1425.143, 1496.167),
    payment_sd     = c(337.074, 233.323, 322.734),
    n              = c(12, 7, 6))

  expect_equal(sm, b)
})