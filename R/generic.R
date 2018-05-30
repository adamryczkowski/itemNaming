#' Generates a string (or table) that adequately describes a set of elements in the natural language.
#'
#' Supports multiple object attributes (that can be used as a secondary names, like \code{element1 (name: Bob), element2 (Charlie) and element3 (Dave)}.'
#' Supports Markdown formatting
#' Supports inverse matching, i.e. \code{All elements except for element20 (name: Ted) and element21 (Unity)}.
#' Supports tables of items for really big lists.
#'
#' Produced string will have a format
#' \code{First item[[<attribute_prefix>Attribute 1 with name]<attribute_comma>|<attribute_and>Attribute 2 with name... <attribute_sufix>]<item_comma>|<item_and>|<item_ellipsis>Second item[...]...}
#'
#' where \code{attribute_prefix}, \code{attribute_comma}, \code{attribute_and}, \code{attribute_sufix}, \code{item_comma}, \code{item_and} and \code{item_ellipsis} are arguments to the function.
#'
#' Each item and its attributes share additional arguments, that are input using the \code{field_attributes_df} data frame.
#'
#'
#'
#' @param items_df A vector, data.frame or a list that will be translated into the string. List will be translated into the data.frame using lists2df.
#'              It can also be a list of two equal sized vectors, in which case first vector will give a first name, and second (if not NA) will give the alternate name, that
#'              will be displayed in braces. Names that start with underscore are ignored.
#'              Columns with names that start with underscore followed by name of the existing column are assumed to contain properties of the given column.
#' @param field_attributes_df A data.frame or a list the defines properties of the items. The properties can also be input as attributes of the columns of the items,
#'                         if items is a data.frame.
#' \describe{
#'   \item{label_name}{String with a nice name of the attribute. If specified, it will be preferred over the column name. Ignored on the first column. If empty, the name of the attribute will never be displayed.}
#'   \item{label_name_suffix}{String that will be placed immidiately after the column name. Defaults to \code{' '}. Suggested alternative value: \code{': '}.}
#'   \item{included}{Boolean that determines whether a given item should be included in the list. This allows for the inverted string ("all cases except for ...")}
#'   \item{prefix}{String appended before the item. Can be used to add Markdown quotes \code{'`'}}
#'   \item{suffix}{String appended behind the item. Can be used to add Markdown quotes \code{'`'}}
#'   \item{}
#' }
#' @param attribute_prefix String that will be put before the list of attributes. Defaults to \code{'\uA0('}}
#' @param attribute_sufix String that will be put before the list of attributes. Defaults to \code{')'}}
#' @param attribute_comma String that will put between attributes in the list of the attributes. Defaults to \code{', '}}
#' @param attribute_and String that will put between the last two attributes in the attributes' list. Defaults to \code{' and\uA0'}}
#' @param item_comma String that will put between items. Defaults to \code{', '}}
#' @param item_and String that will put between the last two items. Defaults to \code{' and\uA0'}}
#' @param item_ellipsis Function that will emit string that be put in place of the ommited items, when rendering in ellipsis mode. The function will get the following arguments: \code{total_count}, \code{omited_count}, \code{item_numeral}. Defaults to function that renders string ", and 5 more items"}.
#'                      Possible fields: \code{#item_count_full} - number of ommited elements together with word "item" (customizable elsewhere).
#'                      \code{#item_count_full_more} - number of ommited elements together with phrase "more items", like in "34 more item".
#'                      \code{#item_count_raw} - just integer with number of ommited elements.}
#' @param all_except_infix Function that returns string that will be prefixed before inversed mode. Defaults to \code{'all #item_name_plural except for '}.
#'                         Function takes arguments: \code{item_numeral}, \code{all_items_count}, \code{selected_items_count}.
#' @param item_name Vector of two strings that name singular and plural name of the item. Defaults to \code{c('item','items')}.
#' @param max_explicit_items_count Maximum number of explicit items. -1 is infinity. If number of items in the items_df exceeds that number, function will use
#'                                 the ellipsis. Must be bigger than sum of \code{number_of_elements_around_ellipsis} arguments.
#' @param number_of_elements_around_ellipsis Vector of two integers, defaults to \code{3, 2}. First integer determines number of elements before the ellipsis,
#'                                           and second - after.
#' @param threshold_for_inversion Minimum number of items required to show the inversion. Inversion excludes table.
#'
#'
#' @aliases format_item_list_en
#' @export
step_1_inversion<-function(items_df, all_except_infix = all_except_infix_fn_pl, item_numeral,
                           threshold_for_inversion=0.8, step_2_ellipsis, flag_prevent_ellipsis_on_inversion=TRUE,
                           included_column_name=paste0(getOption('special_column_prefix'), getOption('includes_column_name')))
{
  checkmate::assert_data_table(items_df, types = c('logical', 'integerish', 'numeric', 'character', 'factor', 'list'), null.ok = FALSE)
  checkmate::assert_function(all_except_infix, args = c('item_numeral','all_items_count','selected_items_count'))
  checkmate::assert_class(item_numeral, classes = 'generic_numeral')
  checkmate::assert_number(threshold_for_inversion, upper=1, finite=TRUE)
  checkmate::assert_function(step_2_ellipsis, args = c('items_df', 'item_numeral'))

  # Check for invertion

  flag_invert<-FALSE
  if(included_column_name %in% colnames(items_df)) {
    #Possibility for invertion
    includes<-items_df[[included_column_name]]
    checkmate::assert_logical(includes, any.missing = FALSE)
    if(sum(includes)/length(includes) > threshold_for_inversion) {
      env<-environment(step_2_ellipsis)
      ellipsis_max_explicit_items_count<-env$max_explicit_items_count
      if(flag_prevent_ellipsis_on_inversion && sum(includes)>ellipsis_max_explicit_items_count) {
        items_df<-data.table:::subset.data.table(items_df, includes, select = setdiff(colnames(items_df), included_column_name))
        prefix<-''
      } else {
        items_df<-data.table:::subset.data.table(items_df, !includes, select = setdiff(colnames(items_df), included_column_name))
        prefix<-all_except_infix(item_numeral=item_numeral, all_items_count=length(includes), selected_items_count=sum(includes))
      }
    } else {
      items_df<-data.table:::subset.data.table(items_df, includes, select = setdiff(colnames(items_df), included_column_name))
      prefix<-''
    }
  } else {
    prefix<-''
  }
  ans<-step_2_ellipsis(items_df=items_df, item_numeral=item_numeral)
  out<-paste0(prefix, ans)
  out
}

item_ellipsis_fn_pl<-function(item_numeral, all_items_count, selected_items_count) {
  ans<-paste0(', ', item_numeral$render(selected_items_count), ' więcej, ')
  return(ans)
}

item_ellipsis_fn_en<-function(item_numeral, all_items_count, selected_items_count) {
  ans<-paste0(', ', item_numeral$render(selected_items_count), ' more, ')
  return(ans)
}

all_except_infix_fn_pl<-function(item_numeral, all_items_count, selected_items_count) {
  all_name<-switch (item_numeral$genus,
                    'M' = if(item_numeral$is_human) {
                      'wszyscy'
                    } else {
                      'wszystkie'
                    },
                    'N' = 'wszystkie',
                    'F' = 'wszystkie')
  numeral<-item_numeral$render_noun(number = 2)
  paste0(all_name, " ", numeral, " poza:\uA0")
}

all_except_infix_fn_en<-function(item_numeral, all_items_count, selected_items_count) {
  paste0('all ', item_numeral$render_noun(number = 2), " except for ")
}

step_2_ellipsis_gen<-function(item_ellipsis=item_ellipsis_fn_en,
                          max_explicit_items_count=7, number_of_elements_around_ellipsis=c(3,2),
                          step3_comma_list)
{
  checkmate::assert_function(item_ellipsis, args = c('item_numeral','all_items_count','selected_items_count'))
  checkmate::assert_integerish(number_of_elements_around_ellipsis, len = 2)
  checkmate::assertTRUE(sum(is.na(number_of_elements_around_ellipsis))==0)

  checkmate::assert_int(max_explicit_items_count, na.ok = FALSE, lower = sum(number_of_elements_around_ellipsis))
  checkmate::assert_function(step3_comma_list, args = c('items_df'))



  step_2_ellipsis<-function(items_df, item_numeral) {
    checkmate::expect_data_frame(items_df)
    checkmate::expect_class(item_numeral, 'generic_numeral')
    if(nrow(items_df)>max_explicit_items_count) {
      #We need to use the ellipsis
      left_rows<-seq_len(number_of_elements_around_ellipsis[[1]])
      left_df<-items_df[left_rows,,drop=FALSE]

      right_rows<-seq(nrow(items_df)-number_of_elements_around_ellipsis[[2]]+1, nrow(items_df))
      if(number_of_elements_around_ellipsis[[2]]==0){
        right_rows<-integer(0)
      }
      right_df<-items_df[right_rows,,drop=FALSE]

      middle_rows<-seq(number_of_elements_around_ellipsis[[1]]+1, nrow(items_df)-number_of_elements_around_ellipsis[[2]])
      if(sum(number_of_elements_around_ellipsis)>=nrow(items_df)) {
        browser()
      }
      middle_df<-items_df[middle_rows,,drop=FALSE]

      ansl<-step3_comma_list(left_df)
      ansm<-item_ellipsis(item_numeral=item_numeral, all_items_count=nrow(items_df), selected_items_count=nrow(middle_df))
      ansr<-step3_comma_list(right_df)
      return(paste0(ansl, ansm, ansr))
    } else {
      #Just bypass
      ans<-step3_comma_list(items_df)
      return(ans)
    }
  }
  return(step_2_ellipsis)
}

step_3_comma_gen<-function(txt_separator=', ', txt_separator_last = ' oraz ', display_item_fn=display_item) {
  checkmate::assert_function(display_item_fn, args=c('items_df', 'rownr'))
  #Function that renders comma-separated list of numerals, together with attributes.
  display_item_fn<-display_item_fn
  step_3_comma<-function(items_df) {
    flag_first=TRUE
    flag_last=FALSE
    #browser()
    out<-''
    for(i in seq_len(nrow(items_df))) {
      if(i == nrow(items_df)){
        flag_last<-TRUE
      }
      if(flag_first) {
        flag_first<-FALSE
      } else {
        if(flag_last) {
          out<-paste0(out, txt_separator_last)
        } else {
          out<-paste0(out, txt_separator)
        }
      }
      #cat(paste0('step ', i, '; out: ', out, '\n'))
      out<-paste0(out, display_item_fn(items_df, i))
    }
    return(out)
  }
  return(step_3_comma)
}



#Displays a single item with the list of attributes
step_4_display_item_gen<-function(txt_attribute_separator=', ', txt_attribute_separator_last=' i ',
                                  txt_attribute_prefix=' (', txt_attribute_suffix=')',
                                  txt_attribute_bare_quote = '`', txt_attribute_label_quote = '', txt_attribute_infix = ':\uA0',
                                  step_5_display_element=step_5_display_single_item) {
  checkmate::assert_string(txt_attribute_separator)
  checkmate::assert_string(txt_attribute_separator_last)
  checkmate::assert_string(txt_attribute_prefix)
  checkmate::assert_string(txt_attribute_suffix)
  checkmate::assert_string(txt_attribute_bare_quote)
  checkmate::assert_string(txt_attribute_label_quote)
  checkmate::assert_string(txt_attribute_infix)
  checkmate::assert_function(step_5_display_element, args = c('value','properties'))


  step_4_display_item<-function(items_df, rownr) {
    checkmate::assert_int(rownr, na.ok = FALSE)

    col_attr_prefix<-getOption('attribute_column_prefix')
    all_col_list<-colnames(items_df)
    col_list<-all_col_list[stringr::str_detect(all_col_list,
                                               stringr::regex(paste0('^(?!', col_attr_prefix, '.*$).*')))]
    col_attr<-stringr::str_match(all_col_list, stringr::regex(paste0('^', col_attr_prefix,'(?!', col_attr_prefix, '.*$)(.*)')))[,2]

    mappings<-purrr::map(col_list, ~which(.==col_attr))
    mappings<-purrr::map_int(mappings, ~switch(min(length(.)+1,3), NA_integer_, ., .[[1]]))
    mappings<-setNames(all_col_list[mappings], col_list)
    if(length(mappings)==0) {
      browser()
    }
    out<-''
    col_attr<-ifelse(is.na(col_attr), NA, paste0('_',col_attr))

    main_name<-names(mappings)[[1]]
    properties<-items_df[[mappings[[main_name]]]][[rownr]]
    if(identical(NA, properties)) {
      properties<-list()
    }
    #Display the main element. It has to handle element given as a one-item list
    out<-step_5_display_element(value=items_df[[main_name]][[rownr]], properties)

    #Only one element, no attributes
    if(length(mappings)==1) {
      return(out) #no attributes - just exit now
    }
    #Display attributes. First generate a df of cases to display:
    mappings<-mappings[setdiff(names(mappings), main_name)]

    if(rownr==1) {
      labels<-Hmisc::label(data.table:::subset.data.table(items_df, , names(mappings)))
      labels<-ifelse(labels=='',
                     paste0(txt_attribute_bare_quote, names(mappings), txt_attribute_bare_quote),
                     paste0(txt_attribute_label_quote, labels, txt_attribute_label_quote)
                     )
    }
    rownr_ext<-rownr
    items_df_ext<-items_df
    flag_attr_exist=FALSE
    for(i in seq_along(mappings)) {
      name<-names(mappings)[[i]]
      value<-items_df[[name]][[rownr]]
      if(!is.na(value)) {
        if(value!='') {
          flag_attr_exist<-TRUE
          break
        }
      }
    }
    if(!flag_attr_exist) {
      return(out) #No actual attributes, although columns exist
    }
    attr_display_step_5_fn<-function(items_df, rownr)
    {
      #For speed we ignore the items_df_ext and use the parent's environment source.
      #So rownr is the index of the mappings.
      name<-names(mappings)[[rownr]]
      value<-items_df_ext[[name]][[rownr_ext]]
      name_<-mappings[[rownr]]
      if(is.na(name_)) {
        properties<-list()
      } else {
        properties<-items_df_ext[[name_]][[rownr_ext]]
      }
      if(rownr_ext==1){
        properties<-c(properties, list(label_prefix=paste0(labels[[rownr]], txt_attribute_infix)))
      }
      step_5_display_element(value=value, properties=properties)
    }
    attr_display_step_4_fn<-step_3_comma_gen(display_item_fn = attr_display_step_5_fn, txt_separator = txt_attribute_separator, txt_separator_last = txt_attribute_separator_last)
    ans<-attr_display_step_4_fn(data.frame(a=seq_along(mappings)))
    out<-paste0(out, txt_attribute_prefix, ans, txt_attribute_suffix)
    return(out)
  }
}


#step 5 - displays a single item, formatted with additional properties' list
step_5_display_single_item<-function(value, properties) {
  label_prefix<-properties$label_prefix
  if(!is.null(label_prefix)){
    ans<-label_prefix
    properties$label_prefix<-NULL
  } else {
    ans<-''
  }
  label<-properties$label
  if(!is.null(label)) {
    if(label!='') {
      quotes<-properties$label_quote
      if(is.null(quotes)) {
        quotes<-''
      }
      value<-label
    } else {
      label<-NULL
    }
  }
  if(is.null(label))  {
    quotes<-properties$bare_quote
    if(is.character(value)) {
      if(is.null(quotes)) {
        quotes<-'`'
      }
    } else {
      if(is.null(quotes)) {
        quotes<-''
      }
    }
  }
  properties<-properties[setdiff(names(properties), c('label_prefix','label','label_quote','bare_quote'))]
  for(i in seq_along(properties)) {
    attr(value, names(properties)[[i]])<-properties[[i]]
  }
  ans<-paste0(ans, quotes,  format_values(value), quotes)
  return(ans)
}


# wykorzystywane atrybuty kolumn df:
# * label - nazwa danego atrybutu
# * decoration - znak, którym należy dekorować daną kolumną przed i po
# * decoration_prefix
# * decoration_suffix - zamiast decoration.
# * factor_sprintf_format - format zostanie użyty, jeśli zmienna jest typu factor. domyślnie: "%2$s (%1$s)"
# * verbatim - jeśli ustawiona to wartość nie zostanie w żaden sposób formatowana
# * infix_equal - jaki znak użyć przy cytowaniu wartości danego atrybutu?
#   Np. " = " albo ": ", co da "[coś] = 23" albo "[coś]: 23"
#
# colname_with_includes - musi wskazywać na kolumnę o nazwie istniejącej w df
# percent_for_inversion - gdy liczba elementów do pokazania przekroczy threshold_for_table i będzie
#                         większa od percent_for_inversion% wszystkich rekordów, i została wskazana
#                         colname_with_includes, to zostanie reportowane
#                         dopełnienie zbioru rekordów.
# flag_use_ellpisis - gdy do przedstawienia będzie więcej, niż threshold_for_table elementów,
#                     to zostanie użyty długi format w formie niedokończonej listy (z ...).
#                     W przeciwnym razie, zostanie wklejona tabela.
# txt_attribute_separator - przydatne, gdy generujemy słowa a nie tabelkę.
# threshold_for_table     - Jeśli liczba elementów przekroczy ten próg, to spróbujemy zrobić tabelę (lub notację
#                           odwróconą, jeśli podany zbiór df zawiera wszystkie potencjalne rekordy, a nie
#                           tylko te, które chcemy wypisać, z kolumną filtrującą wskazaną przez argument includes)
#
#Przykład:
#> df<-tibble(bla=as.character(1:5))
#> danesurowe::format_item_list(df = df)
# [1] "1, 2, 3, 4 oraz 5"


format_item_list_en<-function(items, field_attributes, colname_with_includes=NULL, includes=NULL,  flag_use_ellipsis = FALSE,
                              threshold_for_table=10, percent_for_inversion=0.8,
                              table_caption=NULL, table_prefix='\n\n', text_prefix='',
                              txt_attribute_separator=', ', txt_attribute_separator_last=' and ',
                              txt_attribute_prefix=' (', txt_attribute_suffix=')',
                              txt_separator=', ', txt_separator_last = ' and ',
                              txt_attribute_infix = ':\uA0', txt_attribute_bare_quote = '`',
                              txt_attribute_label_quote = '',
                              prefix_all_except = 'all except: ') {
  format_item_list(items=items, field_attributes=field_attributes, colname_with_includes=colname_with_includes, includes=includes,
                   flag_use_ellipsis = flag_use_ellipsis, threshold_for_table=threshold_for_table,
                   percent_for_inversion=percent_for_inversion, table_caption=table_caption,
                   table_prefix=table_prefix, text_prefix=text_prefix,
                   txt_attribute_separator=txt_attribute_separator,
                   txt_attribute_separator_last=txt_attribute_separator_last,
                   txt_attribute_prefix=txt_attribute_prefix, txt_attribute_suffix=txt_attribute_suffix,
                   txt_separator=txt_separator, txt_separator_last = txt_separator_last,
                   txt_attribute_infix = txt_attribute_infix, txt_attribute_bare_quote = txt_attribute_bare_quote,
                   txt_attribute_label_quote = txt_attribute_label_quote,
                   prefix_all_except = prefix_all_except)
}

#' Generates a string (or table) that adequately describes a set of elements in the natural language.
#'
#' Supports multiple object attributes (that can be used as a secondary names, like \code{element1 (name: Bob), element2 (Charlie) and element3 (Dave)}.'
#' Supports Markdown formatting
#' Supports inverse matching, i.e. \code{All elements except for element20 (name: Ted) and element21 (Unity)}.
#' Supports tables of items for really big lists.
#'
#' Produced string will have a format
#' \code{First item[[<attribute_prefix>Attribute 1 with name]<attribute_comma>|<attribute_and>Attribute 2 with name... <attribute_sufix>]<item_comma>|<item_and>|<item_ellipsis>Second item[...]...}
#'
#' where \code{attribute_prefix}, \code{attribute_comma}, \code{attribute_and}, \code{attribute_sufix}, \code{item_comma}, \code{item_and} and \code{item_ellipsis} are arguments to the function.
#'
#' Each item and its attributes share additional arguments, that are input using the \code{field_attributes_df} data frame.
#'
#'
#'
#' @param items_df A vector, data.frame or a list that will be translated into the string. List will be translated into the data.frame using lists2df.
#'              It can also be a list of two equal sized vectors, in which case first vector will give a first name, and second (if not NA) will give the alternate name, that
#'              will be displayed in braces. Names that start with underscore are ignored.
#'              Columns with names that start with underscore followed by name of the existing column are assumed to contain properties of the given column.
#' @param field_attributes_df A data.frame or a list the defines properties of the items. The properties can also be input as attributes of the columns of the items,
#'                         if items is a data.frame.
#' \describe{
#'   \item{label_name}{String with a nice name of the attribute. If specified, it will be preferred over the column name. Ignored on the first column. If empty, the name of the attribute will never be displayed.}
#'   \item{label_name_suffix}{String that will be placed immidiately after the column name. Defaults to \code{' '}. Suggested alternative value: \code{': '}.}
#'   \item{included}{Boolean that determines whether a given item should be included in the list. This allows for the inverted string ("all cases except for ...")}
#'   \item{prefix}{String appended before the item. Can be used to add Markdown quotes \code{'`'}}
#'   \item{suffix}{String appended behind the item. Can be used to add Markdown quotes \code{'`'}}
#'   \item{}
#' }
#' @param attribute_prefix String that will be put before the list of attributes. Defaults to \code{'\uA0('}}
#' @param attribute_sufix String that will be put before the list of attributes. Defaults to \code{')'}}
#' @param attribute_comma String that will put between attributes in the list of the attributes. Defaults to \code{', '}}
#' @param attribute_and String that will put between the last two attributes in the attributes' list. Defaults to \code{' and\uA0'}}
#' @param item_comma String that will put between items. Defaults to \code{', '}}
#' @param item_and String that will put between the last two items. Defaults to \code{' and\uA0'}}
#' @param item_ellipsis Function that will emit string that be put in place of the ommited items, when rendering in ellipsis mode. The function will get the following arguments: \code{total_count}, \code{omited_count}, \code{item_numeral}. Defaults to function that renders string ", and 5 more items"}.
#'                      Possible fields: \code{#item_count_full} - number of ommited elements together with word "item" (customizable elsewhere).
#'                      \code{#item_count_full_more} - number of ommited elements together with phrase "more items", like in "34 more item".
#'                      \code{#item_count_raw} - just integer with number of ommited elements.}
#' @param all_except_infix String that will be prefixed before inversed mode. Defaults to \code{'all #item_name_plural except for '}
#' @param item_name Vector of two strings that name singular and plural name of the item. Defaults to \code{c('item','items')}.
#' @param max_explicit_items_count Maximum number of explicit items. -1 is infinity. If number of items in the items_df exceeds that number, function will use
#'                                 the ellipsis. Must be bigger than sum of \code{number_of_elements_around_ellipsis} arguments.
#' @param number_of_elements_around_ellipsis Vector of two integers, defaults to \code{3, 2}. First integer determines number of elements before the ellipsis,
#'                                           and second - after.
#' @param threshold_for_inversion Minimum number of items required to show the inversion. Inversion excludes table.
#'
#'
#' @aliases format_item_list_en
#' @export
decide_inversion<-function(items_df, field_attributes_df, all_except_infix = 'all #item_name_plural except for ',
                                        attribute_prefix='\uA0(', attribute_sufix=')', attribute_comma=', ', attribute_and=' and\uA0',
                                        item_comma=', ', item_and=' and\uA0', item_ellipsis=', #item_count_full_more#, ', item_name = c('item','items'),
                                        max_explicit_items_count=7, number_of_elements_around_ellipsis=c(3,2), threshold_for_inversion=0.8)
{
  item_name<-colnames(df)[[1]]
  checkmate::assert_data_table(items_df, types = c('logical', 'integerish', 'numeric', 'character', 'factor'), null.ok = FALSE)
  checkmate::assert_data_table(field_attributes_df, types = c('numeric', 'character'), null.ok = FALSE)
  checkmate::assert_subset(colnames(field_attributes_df), choices = c('label_name', #Nice name of the attribute. If specified, it will be preferred over the column name.
                                                                      'label_name_suffix', #String that will be placed immidiately after the column name. Defaults to ' '. Suggested alternative value: ': '.
                                                                      'prefix', #String appended before the item. Can be used to add Markdown quotes '`'
                                                                      'suffix')) #String appended behind the item. Can be used to add Markdown quotes '`'


  # Check for invertion

  includes_colname<-paste0(getOption('special_column_prefix'), getOption('includes_column_name'))
  flag_invert<-FALSE
  if(includes_colname %in% colnames(items_df)) {
    #Possibility for invertion
    includes<-items_df[[includes_colname]]
    checkmate::assert_logical(includes, any.missing = FALSE)
    if(sum(includes)/length(includes) > threshold_for_inversion) {
      items_df<-data.table:::subset.data.table(items_df, !includes, select = setdiff(colnames(items_df), includes_colname))
      prefix<-render_text_fields(text=all_except_infix, number=length(includes), item_name=item_name)

    } else {
      items_df<-data.table:::subset.data.table(items_df, includes, select = setdiff(colnames(items_df), includes_colname))
    }
  }
  out<-decide_ellipsis()


}

render_text_fields<-function(text, number=NA, item_name, language='EN') {
  checkmate::assert_character(text, any.missing = FALSE)
  checkmate::assert_character(item_name, len = 2, any.missing = FALSE)
  if(!is.na(number)) {
    text<-
      stringr::str_replace_all(text, pattern=stringr::fixed('#item_count_raw'), replacement = format_integer(number))
    text<-
      stringr::str_replace_all(text, pattern=stringr::fixed('#item_count_full'), replacement = numeral(number, item_name, language=language))

  }
}

format_item_list<-function(items, field_attributes=list(), colname_with_includes=NULL, includes=NULL,  flag_use_ellipsis = FALSE,
                           threshold_for_table=10, percent_for_inversion=0.8,
                           table_caption=NULL, table_prefix='\n\n', text_prefix='',
                           txt_attribute_separator=', ', txt_attribute_separator_last=' i ',
                           txt_attribute_prefix=' (', txt_attribute_suffix=')',
                           txt_separator=', ', txt_separator_last = ' oraz ',
                           txt_attribute_infix = ':\uA0', txt_attribute_bare_quote = '`',
                           txt_attribute_label_quote = '',
                           prefix_all_except = 'wszystkie poza: '
)
{
  if('list' %in% class(items)) {
    if(length(items)==0) {
      return('')
    }
    el<-items[[1]]
    df<-lists2df::lists_to_df(items)
    if('list' %in% class(el)) {
      #List of lists
      colnames(df)<-'item'
    }
  } else if(is.atomic(items)) {
    df<-tibble::tibble(item=items)
  } else {
    df<-items
  }

  item_name<-colnames(df)[[1]]
  checkmate::assert_data_frame(df, types = c('logical', 'integerish', 'numeric', 'character', 'factor'), null.ok = FALSE)


  if('list' %in% class(field_attributes)) {
    if(length(field_attributes)==0) {
      field_attributes<-tibble::tibble(item=list())
    }
  }
  if('list' %in% class(field_attributes)) {
    if(ncol(df)==1) {
      field_attributes<-list(item=field_attributes)
    }
    field_attributes_df<-lists2df::lists_to_df(field_attributes)
  } else {
    field_attributes_df<-field_attributes
  }
  checkmate::assert_data_frame(field_attributes_df, types = c('numeric', 'character'), null.ok = FALSE)
  checkmate::assert_subset(colnames(field_attributes_df), choices = c('label', #Nice name of the attribute. If specified, it will be preferred over the column name.
                                                                      'decoration', #String that will put before and after each element
  ''))


  if (!is.null(colname_with_includes)) {
    if(length(colname_with_includes)!=1)
      stop("colname_with_includes argument must have length 1")
    if(! colname_with_includes %in% colnames(items)){
      stop(paste0("Cannot find colname_with_includes=",colname_with_includes,' among field names in items'))
    }
    if(!is.null(includes)) {
      stop(paste0("You cannot put both includes and colname_with_includes"))
    }
    includes <- df[[colname_with_includes]]
    if('data.table' %in% class(df)){
      df <- copy(df)
    }
    df[[colname_with_includes]] <- NULL
    flag_can_use_inverse <- TRUE
  } else {
    if(is.null(includes))  {
      includes <- rep(TRUE, nrow(df))
      flag_can_use_inverse <- FALSE
    } else {
      if(length(includes)!=nrow(df)) {
        stop("Vector includes must have the same lenght as number of rows of the dataframe to display")
      }
      flag_can_use_inverse <- TRUE
    }
  }

  #See if we should produce the inverted string

  if(sum(includes)>threshold_for_table) {
    if(flag_can_use_inverse) {
      if(sum(includes)/nrow(df) > percent_for_inversion) {
        dfinv<-df[!includes,]
        copy_dt_attributes(dt_source = df, dfinv)
        ans<-paste0(prefix_all_except,
                    format_item_list(dfinv, flag_use_ellipsis=flag_use_ellipsis,
                                     txt_attribute_separator=txt_attribute_separator,
                                     txt_attribute_separator_last=txt_attribute_separator_last,
                                     txt_attribute_prefix=txt_attribute_prefix,
                                     txt_attribute_suffix=txt_attribute_suffix,
                                     txt_separator=txt_separator, txt_separator_last = txt_separator_last,
                                     txt_attribute_infix = txt_attribute_infix,
                                     txt_attribute_bare_quote = txt_attribute_bare_quote,
                                     txt_attribute_label_quote = txt_attribute_label_quote,
                                     table_prefix=table_prefix, table_caption=table_caption,
                                     threshold_for_table=threshold_for_table))
        return(ans)
      }
    }
  }
  #See if number of cases is so large, that we need to produce a table
  tabdf<-df[includes,]
  tabdf<-copy_dt_attributes(df, tabdf)
  for(cn in seq_along(colnames(tabdf))) {
    tabdf[[cn]]<-report_values(tabdf[[cn]])
  }
  tabdf<-tibble::as_tibble(tabdf)

  if(sum(includes)>threshold_for_table) {
    if(!flag_use_ellipsis) {
      tabdf <- as.matrix.data.frame(tabdf)
      colnames(tabdf)<-format_colnames(df, quote_bare_name = txt_attribute_bare_quote,
                                       quote_label = txt_attribute_label_quote)
      if(is.null(table_prefix)) {
        table_prefix<-''
      }
      return(paste0('\n', table_prefix, pander::pandoc.table.return(tabdf, caption = table_caption)))
    }
  }

  #Now we can be sure we need to render everything as a non-inverted string

  if(ncol(df)>1) {
    #To znaczy, że mamy format z atrybutami
    fmt_fn <- function(i, df) {
      if(i==1) {
        ans_vec<-format_attr_list(df = df[, 2:ncol(df)], nrow=i, quote_bare_name = txt_attribute_bare_quote,
                                  quote_label = txt_attribute_label_quote, infix = txt_attribute_infix)
      } else {
        ans_vec<-purrr::map_chr(df[i,2:ncol(df)], as.character)
      }
      if(ncol(df)>2) {
        ans<-paste0(df[[1]][[i]], txt_attribute_prefix,
                    paste0(ans_vec[1:(length(ans_vec)-1)], collapse = txt_attribute_separator),
                    txt_attribute_separator_last, ans_vec[[length(ans_vec)]], txt_attribute_suffix)
      } else  {
        ans<-paste0(df[[1]][[i]], txt_attribute_prefix,
                    ans_vec,
                    txt_attribute_suffix)
      }
      return(ans)
    }
    values <- purrr::map_chr(seq(nrow(df)), fmt_fn, df=tabdf)
  } else {
    values <- df[[1]]
  }
  data.table::setattr(values, 'verbatim', 1)

  if(sum(includes)>threshold_for_table) {
    values<-c(values[1:7], '...', values[length(values)-3, length(values)])
  }

  if(length(values)>1) {
    ret<-paste0(paste0(as.character(values[c(-length(values))]), collapse = txt_separator),
                txt_separator_last, values[[length(values)]])
  } else {
    ret<-values
  }
  return(paste0(text_prefix, ret))
}
