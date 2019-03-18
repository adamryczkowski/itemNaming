context("test")
library(itemNaming)
library(testthat)

#source('tests/testthat/testfunctions.R')

#source('testfunctions.R')

testthat::test_that("Simple test for 5th step", {

  testthat::expect_equal(itemNaming:::step_5_display_single_item(value = 123456789, properties=list()), paste0("123","\uA0", "456", "\uA0", "789"))
  testthat::expect_equal(itemNaming:::step_5_display_single_item(value = 1234567000000000000, properties=list()), "1\u{A0}234\u{A0}567\u{A0}000\u{A0}000\u{A0}000\u{A0}000")

})


testthat::test_that("Simple test for 4th step", {
#Displays a single item with the list of attributes
  step_4_display_item<-itemNaming:::step_4_display_item_gen(txt_attribute_separator=', ', txt_attribute_separator_last=' i ',
                                    txt_attribute_prefix=' (', txt_attribute_suffix=')',
                                    txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0',
                                    step_5_display_element=itemNaming:::step_5_display_single_item)


  items_df<-data.table::data.table(
    item=c('Mik', 'Wika', 'Adam', 'Zoś'),
    '_item'=list(list(label="Mikołaj", font=23), list(font=11), list(), list(label='Sofia', font=11)),
    hue=c('Zielony', NA, NA, 'Czewrony'),
    '_hue'=list(list(font=23), list(font=11), list(), list(font=11)),
    wzrost=c(135, 157, 171, 87),
    '_wzrost'=list(list(digits=2), list(digits=1, length=1), list(digits=2, size=12), list(size=11)),
    waga=c(30, 83, 75, NA))

#  debugonce(step_4_display_item)
  out<-step_4_display_item(items_df = items_df, rownr = 1)
  testthat::expect_equal(out, 'Mikołaj (`hue`:\u{A0}`Zielony`, `wzrost`:\u{A0}135 i `waga`:\u{A0}30)')
})


testthat::test_that("Simple test for 3th step", {
  #Displays a single item with the list of attributes
  #debugonce(step_4_display_item_gen)
  step_4_display_item<-itemNaming:::step_4_display_item_gen(txt_attribute_separator=', ', txt_attribute_separator_last=' i ',
                                               txt_attribute_prefix=' (', txt_attribute_suffix=')',
                                               txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0',
                                               step_5_display_element=itemNaming:::step_5_display_single_item)

  step_3_comma<-itemNaming:::step_3_comma_gen(txt_separator=', ', txt_separator_last = ' oraz ', display_item_fn=step_4_display_item)

  items_df<-data.table::data.table(
    item=c('Mik', 'Wika', 'Adam', 'Zoś'),
    '_item'=list(list(label="Mikołaj", font=23), list(font=11), list(), list(label='Sofia', font=11)),
    hue=c('Zielony', NA, NA, 'Czerwony'),
    '_hue'=list(list(font=23), list(font=11), list(), list(font=11)),
    wzrost=c(135, 157, 171, 87),
    '_wzrost'=list(list(digits=2), list(digits=1, length=1), list(digits=2, size=12), list(size=11)),
    waga=c(30, 83, 75, NA))

  #debugonce(step_3_comma)
  out<-step_3_comma(items_df)
  testthat::expect_equal(out, "Mikołaj (`hue`:\u{A0}`Zielony`, `wzrost`:\u{A0}135 i `waga`:\u{A0}30), `Wika` (`NA`, 157 i 83), `Adam` (`NA`, 171 i 75) oraz Sofia (`Czerwony`, 87 i NA)")


  out<-step_4_display_item(items_df = items_df, rownr = 1)
  testthat::expect_equal(out, 'Mikołaj (`hue`:\u{A0}`Zielony`, `wzrost`:\u{A0}135 i `waga`:\u{A0}30)')
})


testthat::test_that("Simple test for 2nd step", {
  #Displays a single item with the list of attributes
  step_4_display_item<-itemNaming:::step_4_display_item_gen(txt_attribute_separator=', ', txt_attribute_separator_last=' i ',
                                               txt_attribute_prefix=' (', txt_attribute_suffix=')',
                                               txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0',
                                               step_5_display_element=itemNaming:::step_5_display_single_item)

  step_3_comma<-itemNaming:::step_3_comma_gen(txt_separator=', ', txt_separator_last = ' oraz ', display_item_fn=step_4_display_item)

  step_2_ellipsis<-itemNaming:::step_2_ellipsis_gen(item_ellipsis=itemNaming:::item_ellipsis_fn_pl,
                                       max_explicit_items_count=3, number_of_elements_around_ellipsis=c(2,1),
                                       step_3_comma)
  item_numeral<-itemNaming:::Polish_numeral$new(mianownik='członek rodziny', dopelniacz='członków rodziny', lmnoga='członkowie rodziny', genus='M',
                                              number_format_fn=format_integer, flag_skip_one = TRUE)


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
  testthat::expect_equal(out, "Mikołaj (`hue`:\u{A0}`Zielony`, `wzrost`:\u{A0}135 i `waga`:\u{A0}30) oraz `Wika` (`NA`, 157 i 83), członek rodziny więcej, Sofia (`hue`:\u{A0}`Czerwony`, `wzrost`:\u{A0}87 i `waga`:\u{A0}NA)")
})


testthat::test_that("Simple test for 1st step", {
  #Displays a single item with the list of attributes
  step_4_display_item<-itemNaming:::step_4_display_item_gen(txt_attribute_separator=', ', txt_attribute_separator_last=' i ',
                                               txt_attribute_prefix=' (', txt_attribute_suffix=')',
                                               txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0',
                                               step_5_display_element=itemNaming:::step_5_display_single_item)

  step_3_comma<-itemNaming:::step_3_comma_gen(txt_separator=', ', txt_separator_last = ' oraz ', display_item_fn=step_4_display_item)

  step_2_ellipsis<-itemNaming:::step_2_ellipsis_gen(item_ellipsis=itemNaming:::item_ellipsis_fn_pl,
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

  item_numeral<-itemNaming:::Polish_numeral$new(mianownik='członek rodziny', dopelniacz='członków rodziny', lmnoga='członkowie rodziny',
                                   number_format_fn=format_integer, flag_skip_one = TRUE, genus = 'M', flag_human=TRUE)
  #debugonce(step_1_inversion)
  out<-itemNaming:::step_1_inversion(items_df, all_except_infix = itemNaming:::all_except_infix_fn_pl, item_numeral=item_numeral,
                        threshold_for_inversion=0.1, step_2_ellipsis, included_column_name='__included')



  #  debugonce(step_2_ellipsis)
  testthat::expect_equal(out, "wszyscy członkowie rodziny poza:\u{A0}`Adam` (`hue`:\u{A0}`NA`, `wzrost`:\u{A0}171 i `waga`:\u{A0}75)")
})
