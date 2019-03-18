format_values<-function(values)
{
  if(!is.null(attr(values,'verbatim', exact = TRUE))) {
    return(values)
  } else {
    UseMethod("format_values", values)
  }
}

get_decorations<-function(values, default='') {
  ans<-rep(default, length.out=2)
  if(!is.null(attr(values,'decoration', exact = TRUE))) {
    ans<-rep(attr(values,'decoration', exact = TRUE),2)
  }
  if(!is.null(attr(values, 'decoration_prefix', exact = TRUE))) {
    ans[[1]]<-attr(values,'decoration_prefix', exact = TRUE)
  }
  if(!is.null(attr(values, 'decoration_suffix', exact = TRUE))) {
    ans[[2]]<-attr(values,'decoration_suffix', exact = TRUE)
  }
  return(ans)
}

format_values.character<-function(values)
{
  decors<-get_decorations(values,'')
  return(paste0(decors[[1]], values, decors[[2]]))
}

format_values.integer<-function(values)
{
  decors<-get_decorations(values,'')
  int_str<-  formattable::comma(values, big.mark='\uA0', digits=0)
  return(paste0(decors[[1]], int_str, decors[[2]]))
}

decimalplaces <- function(x_in) {
  x<-ifelse(abs(x_in - round(x_in)) > .Machine$double.eps^0.5,
            x_in,
            0.1)
  ifelse(abs(x_in - round(x_in)) > .Machine$double.eps^0.5,
              purrr::map_int(stringr::str_match_all(string=as.character(x), pattern = stringr::regex('^.*\\.(.*)$')),~nchar(.[[2]])),
              0)
}

format_values.numeric<-function(values)
{
  dp<-decimalplaces(values)
  int_str<-character(length(values))
  for(i in seq_along(values)) {
    if(is.na(values[[i]])) {
      int_str[[i]]<-haven::format_tagged_na(values[[i]])
    } else {
      int_str[[i]]<-as.character(formattable::comma(values[[i]], big.mark='\uA0', digits=dp))
    }
  }

  decors<-get_decorations(values,'')
  return(paste0(decors[[1]], int_str, decors[[2]]))
}

format_values.Date<-function(values)
{
  decors<-get_decorations(values,'')
  old_locale <- Sys.getlocale(category="LC_TIME")
  Sys.setlocale(category="LC_TIME", locale ="en_US.UTF-8")
  ans<-paste0(decors[[1]], as.character( values, format='%e %b %Y'), decors[[2]])
  Sys.setlocale(category="LC_TIME", locale = old_locale)
  return(ans)
}

format_values.factor<-function(values)
{
  #  browser()
  fmt<-attr(values, 'factor_sprintf_format', exact = TRUE)
  if(is.null(fmt)) {
    fmt<-"%2$s (%1$s)"
  }
  labels<-as.character(values)
  values<-format_values.integer(as.integer(values))
  return(sprintf(fmt, values, labels))
}

format_values.labelled<-function(values)
{
  #  browser()
  out <- as.character(values)

  if(sum(haven::is_tagged_na(values))>0)
  {
    taged_nas<-haven::is_tagged_na(values)
    tags<-haven::na_tag(values)
    NAvalues<-haven::na_tag(labelled::val_labels(values))
    NAlabels<-names(labelled::val_labels(values))[!is.na(NAvalues)]
    NAvalues<-na.omit(NAvalues)
    for(i in seq_along(NAvalues))
    {
      NAvalue <- NAvalues[[i]]
      out[tags==NAvalue]<-paste0("<", NAlabels[[i]], ">")
    }
  } else {
    taged_nas<-rep(FALSE, length(values))
  }
  if(sum(is.na(out))>0)
  {
    out[is.na(out)]<-'NA'
  }

  if(sum(!is.na(values))>0)
  {
    not_nas<-!is.na(values)
    vals<-values[not_nas]
    Lvalues<-labelled::val_labels(values)
    Llabels<-names(labelled::val_labels(values))[!is.na(Lvalues)]
    Lvalues<-na.omit(Lvalues)
    fmt<-attr(values, 'factor_sprintf_format', exact = TRUE)
    if(is.null(fmt)) {
      fmt<-"%2$s (%1$s)"
    }
    for(i in seq_along(Llabels))
    {
      Llabel<-Llabels[[i]]
      Lvalue<-Lvalues[[i]]

      out[values==Lvalue]<-sprintf(fmt, Lvalue, Llabel)
    }
  }
  return(out)
}

