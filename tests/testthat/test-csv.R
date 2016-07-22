context("CSV uploads")
setwd("~/R/funnel")

#
# Testing how the funnels package behaves with various CSV uploads. See fixtures.
#

test_that("successful test", {
  expect_equal(1,1)
})

test_that("problem-free.csv is silent", {
  expect_silent(wrapfunnel(datapath="fixtures/no-problems.csv"))
})

