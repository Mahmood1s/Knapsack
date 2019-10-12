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


context("knapsack_dynamic")

test_that("knapsack dynamic function rejects errornous inout.",{
  expect_error(obj$knapsack_dynamic("hej", 3500))
  expect_error(obj$knapsack_dynamic(obj$ks_dataset[1:8,], -3500))
})

test_that("knapsack dynamic Function return correct results.", {
  bfk <- obj$knapsack_dynamic(obj$ks_dataset[1:8,], 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))
  bfk <- obj$knapsack_dynamic(obj$ks_dataset[1:12,], 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))
  
  bfk <- obj$knapsack_dynamic(obj$ks_dataset[1:8,], 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))
  
  bfk <- obj$knapsack_dynamic(obj$ks_dataset[1:12,], 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))
  
  st <- system.time(bfk <- obj$knapsack_dynamic(obj$ks_dataset[1:16,], 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})


context("greedy_knapsack")

test_that("greedy knapsack function rejects errornous inout.",{
  expect_error(obj$greedy_knapsack("hej", 3500))
  expect_error(obj$greedy_knapsack(obj$ks_dataset[1:8,], -3500))
})

test_that("knapsack greedy Function return correct results.", {
  
  bfk <- obj$greedy_knapsack(obj$ks_dataset[1:800,], 3500)
  expect_equal(round(bfk$value), 194924)
  expect_true(all(round(bfk$elements) %in% c(92,574,472,80,110,537,332,117,37,776,577,288,234,255,500,794,55,290,436,346,282,764,599,303,345,300,243,43,747,35,77,229,719,564,401)))
  bfk <- obj$greedy_knapsack(obj$ks_dataset[1:1200,], 2000)
  expect_equal(round(bfk$value), 213298)
  expect_true(all(round(bfk$elements) %in% c(92,574,472,80,110,840,537,1000,332,117,37,1197,1152,947,904,776,577,288,1147,1131,234,255,1006,833,1176,1092,873,828,1059,500,1090,794,1033,1134)))
  
  bfk <- obj$greedy_knapsack(obj$ks_dataset[1:8,], 3500)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(8,3)))
  
  bfk <- obj$greedy_knapsack(obj$ks_dataset[1:8,], 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(8,3)))
  
  st <- system.time(bfk <- obj$greedy_knapsack(obj$ks_dataset[1:16,], 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})
