.onLoad	<-	function(libname,	pkgname)	{
  op	<-	options()
  op.itemNaming	<-	list(
    attribute_column_prefix = '_',
    special_column_prefix = '__',
    includes_column_name	=	'includes'
  )
  toset	<-	!(names(op.itemNaming)	%in%	names(op))
  if(any(toset))	options(op.itemNaming[toset])
  invisible()
}


#' Generic formatting function. It is the most generic call of the library itemNaming.
#'
#' @param item_numeral Object that knows how to write in a natural language a numeral with a noun. By default it uses an English numeral with the noun "item" (that is capable of creating strings "1 item" or "15 items")
#' @param all_except_infix_fn Function that can create a description of complementary notation in a natural language. The arguments are described later. Default function writes in English text "all items except for", using the \code{item_numeral} for the word "items".
#' @param threshold_for_inversion Number between 0 and 1 that describes the threshold of percentage of selected items to trigger a complementary notation.
#' @param included_column_name Name of the column that contains filtered cases to display. Essential to make the complementary notation possible. Defaults to \code{"__included"}.
#' @param item_ellipsis_fn Function that can create an ellipsis notation in the middle of a long list. Defaults to a function that displays text ", 13 items more, " (assuming the item_numeral is for the noun "item").
#' @param max_explicit_items_count Maximal number of items to be ever displayed in comma separated list. Ellipsis notation will be used, if number of items exceed this value.
#' @param number_of_elements_around_ellipsis Number of elements around the actual ellipsis, when the ellipsis is used. Must be a two element integer vector. Defaults to \code{c(3,2)} to produce text like "item1, item2, item3, 5 items more, item9 and item10"
#' @param txt_separator String that separates list elements for all but last item. Defaults to ", ".
#' @param txt_spearator_last String that separates two last list elements. Defaults to "\\uA0and ". \\uA0 is a unicode for non-breakable space.
#' @param txt_attribute_separator Simmilar to \code{txt_separator}, but for the attributes of a single item (if there are any). Defaults to ", ".
#' @param txt_attribute_separator_last Simmilar to \code{txt_separator_last}, but for the attributes of a single item (if there are any). Defaults to "\\uA0and ".
#' @param txt_attribute_prefix String that gets pasted at the beginning of the attribute list. Defaults to "\\ua0(".
#' @param txt_attribute_suffix String that gets pasted at the beginning of the attribute list. Defaults to ")".
#' @param txt_attribute_bare_quote If the attribute name is available only in its internal name (its label is not specified), this string will get pasted before and after. Defaults to markdown code "`". This may be extended in future to allow formatting in different markup languages, which don't use symmetric quotes, like LaTeX (\code{"\\verbatim\{"} and \\code{"\}"}).
#' @param txt_attribute_label_quote The same as \code{txt_attribute_bare_quote} but for labelled attributes. Defaults to empty string.
#' @param txt_attribute_infix The string that will be inserted between the attribute name and its value. Defaults to ":\\uA0".
#'
#' @return A closure that turns a data.frame into a formatted list (as string)
#' @export
generic_df_formatter_gen<-function(item_numeral=English_numeral$new(singular='item', plural='items', number_format_fn=format_integer, flag_skip_one = FALSE),
                                   all_except_infix_fn=all_except_infix_fn_en, threshold_for_inversion=0.8, included_column_name='__included',
                                   item_ellipsis_fn=item_ellipsis_fn_en, max_explicit_items_count=7, number_of_elements_around_ellipsis=c(3,2),
                                   txt_separator=', ', txt_separator_last = '\uA0and ',
                                   txt_attribute_separator=', ', txt_attribute_separator_last='\uA0and ', txt_attribute_prefix='\uA0(', txt_attribute_suffix=')',
                                   txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0'
) {

  step_4_display_item<-step_4_display_item_gen(txt_attribute_separator=txt_attribute_separator, txt_attribute_separator_last=txt_attribute_separator_last,
                                               txt_attribute_prefix=txt_attribute_prefix, txt_attribute_suffix=txt_attribute_suffix,
                                               txt_attribute_bare_quote = txt_attribute_bare_quote, txt_attribute_label_quote = txt_attribute_label_quote,
                                               txt_attribute_infix = txt_attribute_infix, step_5_display_element=step_5_display_single_item)

  step_3_comma<-step_3_comma_gen(txt_separator=txt_separator, txt_separator_last = txt_separator_last, display_item_fn=step_4_display_item)

  step_2_ellipsis<-step_2_ellipsis_gen(item_ellipsis=item_ellipsis_fn,
                                       max_explicit_items_count=max_explicit_items_count, number_of_elements_around_ellipsis=number_of_elements_around_ellipsis,
                                       step_3_comma)

  gen_fn<-function(items_df) {
    out<-step_1_inversion(items_df=items_df, all_except_infix = all_except_infix_fn, item_numeral=item_numeral,
                          threshold_for_inversion=threshold_for_inversion, step_2_ellipsis, included_column_name=included_column_name)

    return(out)
  }
  return(gen_fn)
}

#' Formatter customized for quoting variables.
#'
#' Parameters are the same as for \code{\link{generic_df_formatter_gen}}, only the default values are slightliy different.
#' The real difference is in the closure returned: it accepts two arguments: \code{varnames} and \code{df}
#' @inherit generic_df_formatter_gen
#' @export
variable_list_formatter_gen<-function(variable_numeral=English_numeral$new(singular='variable', plural='variables', flag_skip_one=FALSE),
                                      flag_include_raw_name=TRUE,
                                      all_except_infix_fn=all_exept_infix_fn_en, threshold_for_inversion=0.8,
                                      item_ellipsis_fn=item_ellipsis_fn_en, max_explicit_items_count=7, number_of_elements_around_ellipsis=c(3,2),
                                      txt_separator=', ', txt_separator_last = ' and ',
                                      txt_attribute_separator=', ', txt_attribute_separator_last=' and ', txt_attribute_prefix=' (', txt_attribute_suffix=')',
                                      txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0') {

  inner_gen<-generic_df_formatter_gen(item_numeral=variable_numeral,
                                      all_except_infix_fn=all_except_infix_fn, threshold_for_inversion=threshold_for_inversion, included_column_name='__included',
                                      item_ellipsis_fn=item_ellipsis_fn, max_explicit_items_count=max_explicit_items_count, number_of_elements_around_ellipsis=number_of_elements_around_ellipsis,
                                      txt_separator=txt_separator, txt_separator_last = txt_separator_last,
                                      txt_attribute_separator=txt_attribute_separator, txt_attribute_separator_last=txt_attribute_separator_last,
                                      txt_attribute_prefix=txt_attribute_prefix, txt_attribute_suffix=txt_attribute_suffix,
                                      txt_attribute_bare_quote = txt_attribute_bare_quote, txt_attribute_label_quote = txt_attribute_label_quote,
                                      txt_attribute_infix = txt_attribute_infix)

  out_gen<-function(varnames, df) {
    checkmate::assert_character(varnames)
    checkmate::assert_data_frame(df)
    varlabels<-
      Hmisc::label(df)
    properties=purrr::map(seq_along(varlabels), ~list(label=varlabels[[.]]))
    rawnames<-colnames(df)
    rawnames[varlabels=='']<-NA
    items_df<-data.table::data.table(var=colnames, '_var'=properties, rawnames=rawnames, '__included'=colnames(df) %in% varnames)
    attr(items_df$rawnames, 'label')<-'Field name'
    inner_gen(items_df)
  }

  return(out_gen)
}

#' @describeIn variable_list_formatter_gen Formatter customized for quoting variables that include data.frame in the closure.
#' @export
variable_list_formatter_df_gen<-function(df, variable_numeral=English_numeral$new(singular='variable', plural='variables', flag_skip_one=FALSE),
                                         flag_include_raw_name=TRUE,
                                         all_except_infix_fn=all_except_infix_fn_en, threshold_for_inversion=0.8,
                                         item_ellipsis_fn=item_ellipsis_fn_en, max_explicit_items_count=7, number_of_elements_around_ellipsis=c(3,2),
                                         txt_separator=', ', txt_separator_last = ' and ',
                                         txt_attribute_separator=', ', txt_attribute_separator_last=' and ', txt_attribute_prefix=' (', txt_attribute_suffix=')',
                                         txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0') {

  inner_gen<-generic_df_formatter_gen(item_numeral=variable_numeral,
                                      all_except_infix_fn=all_except_infix_fn, threshold_for_inversion=threshold_for_inversion, included_column_name='__included',
                                      item_ellipsis_fn=item_ellipsis_fn, max_explicit_items_count=max_explicit_items_count, number_of_elements_around_ellipsis=number_of_elements_around_ellipsis,
                                      txt_separator=txt_separator, txt_separator_last = txt_separator_last,
                                      txt_attribute_separator=txt_attribute_separator, txt_attribute_separator_last=txt_attribute_separator_last,
                                      txt_attribute_prefix=txt_attribute_prefix, txt_attribute_suffix=txt_attribute_suffix,
                                      txt_attribute_bare_quote = txt_attribute_bare_quote, txt_attribute_label_quote = txt_attribute_label_quote,
                                      txt_attribute_infix = txt_attribute_infix)

  varlabels<-Hmisc::label(df)
  varlabels_attr<-varlabels
  rawnames<-colnames(df)
  rawnames_attr<-rawnames

  filter<-varlabels==rawnames
  varlabels[filter]<-NA
  rawnames_attr[filter]<-NA
  rawnames_attr[varlabels=='']<-NA

  properties=purrr::map(seq_along(varlabels), ~list(label=varlabels_attr[[.]]))
  items_df<-data.table::data.table(var=rawnames, '_var'=properties, rawnames=rawnames_attr)
  attr(items_df$rawnames, 'label')<-'Field name'
  out_gen<-function(varnames) {
    data.table::set(items_df, NULL, '__included', rawnames %in% varnames)
    inner_gen(items_df)
  }

  rm(filter, varlabels_attr, rawnames_attr)
  rm(properties)
  rm(varlabels)
  return(out_gen)
}

#' Formatter customized for cases in the data.frame.
#'
#' Parameters are the same as for \code{\link{generic_df_formatter_gen}}, only the default values are slightliy different.
#' The real difference is in the closure returned: it accepts casenames (either string or integers). The formatter outputs all variables
#' encountered in the data.frame, so remove all unwanted variables before supply the data.frame.
#'
#' @inherit generic_df_formatter_gen
#' @export
case_list_formatter_df_gen<-function(df, case_name_var=NULL, case_names=NULL, attr_rows=character(0),  variable_numeral=English_numeral$new(singular='case', plural='cases', flag_skip_one=FALSE),
                                         flag_include_raw_name=TRUE,
                                         all_except_infix_fn=all_except_infix_fn_en, threshold_for_inversion=0.8,
                                         item_ellipsis_fn=item_ellipsis_fn_en, max_explicit_items_count=7, number_of_elements_around_ellipsis=c(3,2),
                                         txt_separator=', ', txt_separator_last = ' and ',
                                         txt_attribute_separator=', ', txt_attribute_separator_last=' and ', txt_attribute_prefix=' (', txt_attribute_suffix=')',
                                         txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0') {

  inner_gen<-generic_df_formatter_gen(item_numeral=variable_numeral,
                                      all_except_infix_fn=all_except_infix_fn, threshold_for_inversion=threshold_for_inversion, included_column_name='__included',
                                      item_ellipsis_fn=item_ellipsis_fn, max_explicit_items_count=max_explicit_items_count, number_of_elements_around_ellipsis=number_of_elements_around_ellipsis,
                                      txt_separator=txt_separator, txt_separator_last = txt_separator_last,
                                      txt_attribute_separator=txt_attribute_separator, txt_attribute_separator_last=txt_attribute_separator_last,
                                      txt_attribute_prefix=txt_attribute_prefix, txt_attribute_suffix=txt_attribute_suffix,
                                      txt_attribute_bare_quote = txt_attribute_bare_quote, txt_attribute_label_quote = txt_attribute_label_quote,
                                      txt_attribute_infix = txt_attribute_infix)

  if(!is.null(case_name_var)) {
    checkmate::assert_string(case_name_var)
    if(!is.null(case_names)) {
      stop("Cannot take both case_name_var and case_names arguments.")
    }
    if(!case_name_var %in% colnames(df)){
      browser()
    }
    case_names<-df[[case_name_var]]
  } else if(!is.null(case_names)) {
    checkmate::assert_character(case_names, unique = TRUE)
    if(length(case_names)!=nrow(df)) {
      stop("case_names must have the same length as number of rows in df")
    }
  } else {
    case_names<-case.names(df)
  }

  items_df<-data.table::data.table(cases=case_names)
  attr_rows<-attr_rows[colnames(df) %in% attr_rows]
  if(length(attr_rows)>0) {
    #Adding other columns as custom attributes
    data.table::set(items_df,NULL, data.table:::subset.data.table(df, NULL, attr_rows))
  }
  out_gen<-function(casenames) {
    if(is.numeric(casenames)) {
      data.table::set(items_df, NULL, '__included', seq_len(nrow(df))  %in% casenames)
    } else {
      data.table::set(items_df, NULL, '__included', case_names  %in% casenames)
    }
    inner_gen(items_df)
  }

  rm(attr_rows)
  return(out_gen)
}

#' Formatter customized for quoting elements from (e.g. character) vector.
#'
#' Parameters are the same as for \code{\link{generic_df_formatter_gen}} except it doesn't support the complementary format.
#'
#' @return Closure that takes a vector and produces a formatted string.
#'
#'
#' @inherit generic_df_formatter_gen
#' @export
vector_formatter_df_gen<-function(variable_numeral=English_numeral$new(singular='element', plural='elements', flag_skip_one=FALSE),
                                     item_ellipsis_fn=item_ellipsis_fn_en, max_explicit_items_count=9, number_of_elements_around_ellipsis=c(4,3),
                                     txt_separator=', ', txt_separator_last = ' and ',
                                     txt_attribute_separator=', ', txt_attribute_separator_last=' and ', txt_attribute_prefix=' (', txt_attribute_suffix=')',
                                     txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0') {
  inner_gen<-generic_df_formatter_gen(item_numeral=variable_numeral,
                                      item_ellipsis_fn=item_ellipsis_fn, max_explicit_items_count=max_explicit_items_count, number_of_elements_around_ellipsis=number_of_elements_around_ellipsis,
                                      txt_separator=txt_separator, txt_separator_last = txt_separator_last,
                                      txt_attribute_separator=txt_attribute_separator, txt_attribute_separator_last=txt_attribute_separator_last,
                                      txt_attribute_prefix=txt_attribute_prefix, txt_attribute_suffix=txt_attribute_suffix,
                                      txt_attribute_bare_quote = txt_attribute_bare_quote, txt_attribute_label_quote = txt_attribute_label_quote,
                                      txt_attribute_infix = txt_attribute_infix)
  out_gen<-function(vector) {
    items_df<-data.table::data.table(items=vector)
    attr(items_df$items, 'label')<-variable_numeral$render_noun(1)
    out<-inner_gen(items_df)
    return(out)
  }
  return(out_gen)
}

