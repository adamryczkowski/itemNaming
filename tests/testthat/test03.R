context("test")
library(itemNaming)
library(testthat)

#source('tests/testthat/testfunctions.R')

#source('testfunctions.R')


testthat::test_that("Simple test for 4th step", {
#Displays a single item with the list of attributes
  step_4_display_item<-step_4_display_item_gen(txt_attribute_separator=', ', txt_attribute_separator_last=' and ',
                                    txt_attribute_prefix=' (', txt_attribute_suffix=')',
                                    txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0',
                                    step_5_display_element=step_5_display_single_item)


  items_df<-data.table::data.table(
    item=c('Mik', 'Wika', 'Adam', 'Zoś'),
    '_item'=list(list(label="Mikołaj", font=23), list(font=11), list(), list(label='Sofia', font=11)),
    hue=c('Zielony', NA, NA, 'Czewrony'),
    '_hue'=list(list(font=23), list(font=11), list(), list(font=11)),
    wzrost=c(135, 157, 171, 87),
    '_wzrost'=list(list(digits=2), list(digits=1, length=1), list(digits=2, size=12), list(size=11)),
    waga=c(30, 83, 75, NA))

  out<-step_4_display_item(items_df = items_df, rownr = 1)
  testthat::expect_equal(out, 'Mikołaj (`hue`: `Zielony`, `wzrost`: 135 and `waga`: 30)')
})


testthat::test_that("Simple test for 3th step", {
  #Displays a single item with the list of attributes
  step_4_display_item<-step_4_display_item_gen(txt_attribute_separator=', ', txt_attribute_separator_last=' and ',
                                               txt_attribute_prefix=' (', txt_attribute_suffix=')',
                                               txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0',
                                               step_5_display_element=step_5_display_single_item)

  step_3_comma<-step_3_comma_gen(txt_separator=', ', txt_separator_last = ' & ', display_item_fn=step_4_display_item)

  items_df<-data.table::data.table(
    item=c('Mik', 'Wika', 'Adam', 'Zoś'),
    '_item'=list(list(label="Mikołaj", font=23), list(font=11), list(), list(label='Sofia', font=11)),
    hue=c('Zielony', NA, NA, 'Czerwony'),
    '_hue'=list(list(font=23), list(font=11), list(), list(font=11)),
    wzrost=c(135, 157, 171, 87),
    '_wzrost'=list(list(digits=2), list(digits=1, length=1), list(digits=2, size=12), list(size=11)),
    waga=c(30, 83, 75, NA))

  out<-step_3_comma(items_df)
  testthat::expect_equal(out, "Mikołaj (`hue`: `Zielony`, `wzrost`: 135 and `waga`: 30), `Wika` (`NA`, 157 and 83), `Adam` (`NA`, 171 and 75) & Sofia (`Czerwony`, 87 and NA)")


  out<-step_4_display_item(items_df = items_df, rownr = 1)
  testthat::expect_equal(out, 'Mikołaj (`hue`: `Zielony`, `wzrost`: 135 and `waga`: 30)')
})


testthat::test_that("Simple test for 1st step", {
  #Displays a single item with the list of attributes
  step_4_display_item<-step_4_display_item_gen(txt_attribute_separator=', ', txt_attribute_separator_last=' and ',
                                               txt_attribute_prefix=' (', txt_attribute_suffix=')',
                                               txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0',
                                               step_5_display_element=step_5_display_single_item)

  step_3_comma<-step_3_comma_gen(txt_separator=', ', txt_separator_last = ' & ', display_item_fn=step_4_display_item)

  step_2_ellipsis<-step_2_ellipsis_gen(item_ellipsis=item_ellipsis_fn_en,
                                       max_explicit_items_count=3, number_of_elements_around_ellipsis=c(2,1),
                                       step_3_comma)
  item_numeral<-English_numeral$new(singular='family member', plural='family members',
                                              number_format_fn=format_integer, flag_skip_one = FALSE)


  items_df<-data.table::data.table(
    item=c('Mik', 'Wika', 'Adam', 'Zoś'),
    '_item'=list(list(label="Mikołaj", font=23), list(font=11), list(), list(label='Sofia', font=11)),
    hue=c('Zielony', NA, NA, 'Czerwony'),
    '_hue'=list(list(font=23), list(font=11), list(), list(font=11)),
    wzrost=c(135, 157, 171, 87),
    '_wzrost'=list(list(digits=2), list(digits=1, length=1), list(digits=2, size=12), list(size=11)),
    waga=c(30, 83, 75, NA))

#  debugonce(step_2_ellipsis)
  out<-step_2_ellipsis(items_df,item_numeral)
  testthat::expect_equal(out, "Mikołaj (`hue`: `Zielony`, `wzrost`: 135 and `waga`: 30) & `Wika` (`NA`, 157 and 83), 1 family member more, Sofia (`hue`: `Czerwony`, `wzrost`: 87 and `waga`: NA)")
})


testthat::test_that("Simple test for 2nd step", {
  #Displays a single item with the list of attributes
  step_4_display_item<-step_4_display_item_gen(txt_attribute_separator=', ', txt_attribute_separator_last=' and ',
                                               txt_attribute_prefix=' (', txt_attribute_suffix=')',
                                               txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0',
                                               step_5_display_element=step_5_display_single_item)

  step_3_comma<-step_3_comma_gen(txt_separator=', ', txt_separator_last = ' & ', display_item_fn=step_4_display_item)

  step_2_ellipsis<-step_2_ellipsis_gen(item_ellipsis=item_ellipsis_fn_en,
                                       max_explicit_items_count=3, number_of_elements_around_ellipsis=c(2,1),
                                       step_3_comma)
  items_df<-data.table::data.table(
    item=c('Mik', 'Wika', 'Adam', 'Zoś'),
    '_item'=list(list(label="Mikołaj", font=23), list(font=11), list(), list(label='Sofia', font=11)),
    hue=c('Zielony', NA, NA, 'Czerwony'),
    '_hue'=list(list(font=23), list(font=11), list(), list(font=11)),
    wzrost=c(135, 157, 171, 87),
    '_wzrost'=list(list(digits=2), list(digits=1, length=1), list(digits=2, size=12), list(size=11)),
    waga=c(30, 83, 75, NA),
    '__included'=c(TRUE, TRUE, FALSE, TRUE))

  item_numeral<-English_numeral$new(singular='family member', plural='family members',
                                    number_format_fn=format_integer, flag_skip_one = FALSE)
  debugonce(step_1_inversion)
  out<-step_1_inversion(items_df, all_except_infix = all_except_infix_fn_en, item_numeral=item_numeral,
                        threshold_for_inversion=0.1, step_2_ellipsis, included_column_name='__included')



  #  debugonce(step_2_ellipsis)
  testthat::expect_equal(out, "all family members except for `Adam` (`hue`: `NA`, `wzrost`: 171 and `waga`: 75)")
})
