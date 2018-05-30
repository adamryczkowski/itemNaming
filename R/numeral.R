
format_integer<-function(x) {
  format_values(as.integer(x))
}

generic_numeral<-R6::R6Class(
  classname='generic_numeral',
  public = list(
    initialize = function(number_format_fn=format_integer, flag_skip_zero=TRUE, flag_skip_one=FALSE, flag_numbers_as_text=FALSE) {
      checkmate::assert_function(number_format_fn)
      private$number_format_fn_<-number_format_fn

      checkmate::assert_flag(flag_skip_zero)
      private$flag_skip_zero_<-flag_skip_zero

      checkmate::assert_flag(flag_skip_one)
      private$flag_skip_one_<-flag_skip_one

      checkmate::assert_flag(flag_numbers_as_text)
      private$flag_numbers_as_text_<-flag_numbers_as_text
    },
    render_noun=function(number=1) {
      browser() #Pure virtual function
    },
    render_number=function(number) { #Renders only numerical representation of the number
      private$number_format_fn_(number)
    },
    render_number_as_text=function(number) { #Renders numbers as text
      browser() #Virtual function that renders numbers as text
    },
    render=function(number) {
      browser() #Pure virtual function
    }
  ),
  private = list(
    number_format_fn_=NULL,
    flag_skip_zero_=NA,
    flag_skip_one_=NA,
    flag_numbers_as_text_=NA
  )
)


#' This says the number in Polish language
liczba_slowami<-function(liczba, flag_skip_zero=FALSE) {
  if(liczba<0) {
    stop("Ujemne liczebniki nie są zaimplementowane")
  }
  if(length(liczba)>1)
  {
    stop("Nie można podać więcej, niż jednej liczby")
  }
  if(liczba!=as.integer(liczba)) {
    stop("Ułamki nie są zaimplementowane")
  }
  if (flag_skip_zero && liczba ==0 ) {
    return("")
  }

  cyfry<-c("zero","jeden","dwa", "trzy", "cztery", "pięć", "sześć", "siedem", "osiem", "dziewięć",
           "dziesięć", "jedenaście", "dwanaście", "trzynaście", "czternaście", "piętnaście", "szesnaście", "siedemnaście", "osiemnaście", "dziwiętnaście")
  dziesiątki <- c("dwadzieścia", "trzydzieści", "czterdzieści", "pięćdziesiąt", "sześćdziesiąt", "siedemdziesiąt", "osiemdziesiąt", "dziewięćdziesiąt")
  setki <- c("sto", "dwieście", "trzysta", "czterysta", "pięćset", "sześćset", "siedemset", "osiemset", "dziwięćset")

  if(liczba > 10E24) {
    stop("Największa zaimplementowana liczba to 999 * 10^21")
  }

  if(liczba < 20) {
    return(cyfry[[liczba+1]])
  }

  if(liczba < 999) {
    c3 <- liczba %/% 100
    liczba <- liczba - 100 * c3
    c2 <- liczba %/% 10
    c1 <- liczba - 10 * c2
    if(c3>0) {
      out<-paste0(setki[[c3]], " ")
    }
    if(liczba<20) {
      return(paste0(out, cyfry[[liczba]]))
    }
    if(c1==0) {
      return(paste0(out, dziesiątki[[c2-1]]))
    } else {
      return(paste0(out, dziesiątki[[c2-1]], " ", cyfry[[c1+1]]))
    }
  }



  miliony1 <- c("tysiąc", "milion", "miliard", "bilion", "biliard", "trylion", "tryliard")
  miliony2 <- c("tysiące", "miliony", "miliardy", "biliony", "biliardy", "tryliony", "tryliardy")
  miliony3 <- c("tysięcy", "milionów", "miliardów", "bilionów", "biliardów", "trylionów", "tryliardów")
  zera <- c(1,seq(3,length(miliony1)*3, 3))
  out<-''
  liczba=1234567890
  while(liczba >0 ) {
    wykl <- as.integer(log10(liczba))+1
    pos<-which(zera-wykl>=0)[[1]]
    c4 <- liczba %/% 10^zera[[pos-1]]
    if (nchar(out)>0) {
      out <- paste0(out, " ")
    }
    if(liczba > 1000) {
      out <- paste0(out, liczebnik(liczba = c4, mianownik = miliony1[[pos-2]], dopelniacz = miliony2[[pos-2]], lmnoga = miliony3[[pos-2]], flag_liczby_slowami = TRUE, flag_skip_one=TRUE))
    } else {
      out <- paste0(out, liczba_slowami(liczba, flag_skip_zero = TRUE))
    }
    liczba <- liczba - c4 * 10^zera[[pos-1]]
  }
  return(out)
}


Polish_numeral<-R6::R6Class(
  classname='Polish_numeral',
  inherit = generic_numeral,
  public = list(
    initialize = function(mianownik, dopelniacz, lmnoga,
                          number_format_fn=format_integer, flag_skip_zero=TRUE, flag_skip_one=FALSE, flag_numbers_as_text=FALSE,
                          genus, flag_human=FALSE) {
      super$initialize(number_format_fn=format_integer, flag_skip_zero=flag_skip_zero, flag_skip_one=flag_skip_one, flag_numbers_as_text=flag_numbers_as_text)
      checkmate::assert_string(mianownik)
      private$mianownik_<-mianownik

      checkmate::assert_string(dopelniacz)
      private$dopelniacz_<-dopelniacz

      checkmate::assert_string(lmnoga)
      private$lmnoga_<-lmnoga

      checkmate::assert_choice(genus, c('M', 'N', 'F'))
      private$genus_<-genus

      checkmate::assert_flag(flag_human)
      private$flag_human_<-flag_human
    },
    render_noun=function(number=1) {
      do_stu <- number %% 100
      if(do_stu==1 && number < 100) {
        return(private$mianownik_)
      } else if (do_stu==1) {
        return(private$dopelniacz_)
      } else if (do_stu<5 && do_stu>0) {
        return(private$lmnoga_)
      } else if (do_stu<22) {
        return(private$dopelniacz_)
      } else {
        cyfra <- number %% 10
        if (cyfra <2 ) {
          return(private$dopelniacz_)
        } else if (cyfra < 5) {
          return(private$lmnoga_)
        } else {
          return(private$dopelniacz_)
        }
      }
      browser("shouldn't be here")
    },
    render_number_as_text=function(number) { #Renders numbers as text
      liczba_slowami(number, flag_skip_zero = private$flag_skip_zero_)
    },
    render=function(number) {
      if (number==0 && private$flag_skip_zero_) {
        return('')
      }
      if (private$flag_skip_one_ && number ==1 ) {
        numtxt=''
      } else {
        if(private$flag_numbers_as_text_) {
          numtxt<-self$render_number_as_text(number=number)
        } else {
          numtxt<-self$render_number(number=number)
        }
        numtxt<-paste0(numtxt, ' ')
      }
      noun<-self$render_noun(number=number)
      return(paste0(numtxt, noun))
    }
  ),
  active = list(
    genus = function() {private$genus_},
    is_human = function() {private$flag_human_}
  ),
  private = list(
    mianownik_=NA,
    dopelniacz_=NA,
    lmnoga_=NA,
    genus_=NA,
    flag_human_=NA
  )
)

English_numeral<-R6::R6Class(
  classname='English_numeral',
  inherit = generic_numeral,
  public = list(
    initialize = function(singular, plural,
                          number_format_fn=format_integer, flag_skip_zero=TRUE, flag_skip_one=FALSE, flag_numbers_as_text=FALSE) {
      super$initialize(number_format_fn=number_format_fn, flag_skip_zero=flag_skip_zero, flag_skip_one=flag_skip_one, flag_numbers_as_text=flag_numbers_as_text)
      checkmate::assert_string(singular)
      private$singular_<-singular

      checkmate::assert_string(plural)
      private$plural_<-plural
    },
    render_noun=function(number=1) {
      if(number>1) {
        return(private$plural_)
      } else {
        return(private$singular_)
      }
    },
    render_number_as_text=function(number) { #Renders numbers as text
      browser()
      #liczba_slowami(number, flag_skip_zero = private$flag_skip_zero_)
    },
    render=function(number) {
      if (number==0 && private$flag_skip_zero_) {
        return('')
      }
      if (private$flag_skip_one_ && number ==1 ) {
        numtxt=''
      } else {
        if(private$flag_numbers_as_text_) {
          numtxt<-self$render_number_as_text(number=number)
        } else {
          numtxt<-self$render_number(number=number)
        }
        numtxt<-paste0(numtxt, ' ')
      }
      noun<-self$render_noun(number=number)
      return(paste0(numtxt, noun))
    }
  ),
  private = list(
    singular_=NA,
    plural_=NA
  )
)
