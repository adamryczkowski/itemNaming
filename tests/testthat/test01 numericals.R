context("Numericals")
library(itemNaming)
library(testthat)

#source('tests/testthat/testfunctions.R')

#source('testfunctions.R')

testthat::test_that("Simple test of numericals", {
    numObj=Polish_numeral$new(mianownik='przedmiot', dopelniacz='przedmiotów', lmnoga='przedmioty', genus='M',
                              number_format_fn=format_integer, flag_skip_one = TRUE)
    ans<-numObj$render(number = 1)
    testthat::expect_equal(ans, 'przedmiot')
    testthat::expect_equal(numObj$render(number = 2), '2 przedmioty')
    testthat::expect_equal(numObj$render(number = 5), '5 przedmiotów')
    testthat::expect_equal(numObj$render(number = 10), '10 przedmiotów')
    testthat::expect_equal(numObj$render(number = 11), '11 przedmiotów')
    testthat::expect_equal(numObj$render(number = 15), '15 przedmiotów')
    testthat::expect_equal(numObj$render(number = 20), '20 przedmiotów')
    testthat::expect_equal(numObj$render(number = 21), '21 przedmiotów')
    testthat::expect_equal(numObj$render(number = 22), '22 przedmioty')
    testthat::expect_equal(numObj$render(number = 100), '100 przedmiotów')
    testthat::expect_equal(numObj$render(number = 101), '101 przedmiotów')
    testthat::expect_equal(numObj$render(number = 102), '102 przedmioty')
    testthat::expect_equal(numObj$render(number = 105), '105 przedmiotów')
    testthat::expect_equal(numObj$render(number = 110), '110 przedmiotów')
    testthat::expect_equal(numObj$render(number = 111), '111 przedmiotów')
    testthat::expect_equal(numObj$render(number = 115), '115 przedmiotów')
    testthat::expect_equal(numObj$render(number = 120), '120 przedmiotów')
    testthat::expect_equal(numObj$render(number = 121), '121 przedmiotów')
    testthat::expect_equal(numObj$render(number = 122), '122 przedmioty')
})


