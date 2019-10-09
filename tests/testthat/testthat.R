context("brute_force_knapsack")

obj <- knapsack_class$new()

test_that("Correct object is returned", {
  expect_silent(bfk <- obj$brute_force_knapsack(obj$ks_dataset[1:8,], 3500))
  expect_named(bfk, c("value", "elements"))
})


test_that("functions rejects errounous input.", {
  expect_error(obj$brute_force_knapsack("hej", 3500))
  expect_error(obj$brute_force_knapsack(obj$ks_dataset[1:8,], -3500))
})

test_that("Function return correct results.", {
  bfk <- obj$brute_force_knapsack(obj$ks_dataset[1:8,], 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))
  bfk <- obj$brute_force_knapsack(obj$ks_dataset[1:12,], 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))
  
  bfk <- obj$brute_force_knapsack(obj$ks_dataset[1:8,], 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))
  
  bfk <- obj$brute_force_knapsack(obj$ks_dataset[1:12,], 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))
  
  st <- system.time(bfk <- obj$brute_force_knapsack(obj$ks_dataset[1:16,], 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})