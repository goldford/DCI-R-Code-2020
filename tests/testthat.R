library(testthat)

# Source tested functions

source("2021 Debug/Alex modifications/Functions/find_exits.R")
source("2021 Debug/Alex modifications/Functions/find_path.R")
source("2021 Debug/Alex modifications/Functions/labeling.R")
source("2021 Debug/Alex modifications/Functions/membership.R")
source("2021 Debug/Alex modifications/Functions/weighted_adj.R")

# Actual testing

test_that("adj_weighted is a matrix representing all river segments", {
  input <- read.csv("2021 Debug/FIPEX_Advanced_DD_2020.csv")
  
  expect_equal(nrow(adj_weighted(FIPEX.table = input, direction = "flow")),
               nrow(input) - 1)
  expect_equal(ncol(adj_weighted(FIPEX.table = input, direction = "flow")),
               nrow(input) - 1)
  expect_equal(nrow(adj_weighted(FIPEX.table = input, direction = "anti-flow")),
               nrow(input) - 1)
  expect_equal(ncol(adj_weighted(FIPEX.table = input, direction = "anti-flow")),
               nrow(input) - 1)
  expect_equal(nrow(adj_weighted(FIPEX.table = input, direction = NULL)),
               nrow(input) - 1)
  expect_equal(ncol(adj_weighted(FIPEX.table = input, direction = NULL)),
               nrow(input) - 1)
  
})

test_that("adj_weighted weighting makes sense", {
  
  
})