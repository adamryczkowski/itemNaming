context("test")
library(itemNaming)
library(testthat)

#source('tests/testthat/testfunctions.R')

#source('testfunctions.R')


testthat::test_that("Simple test for vector of characters", {
#Displays a single item with the list of attributes
  step_4_display_item<-itemNaming:::vector_formatter_df_gen()
  out<-step_4_display_item(vector = c('one', 'two', 'three'))

  testthat::expect_equal(out, '`one`, `two` and `three`')
})

