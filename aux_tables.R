

# -------------------------------------------------------------------------------
# Auxiliar tables
# -------------------------------------------------------------------------------

# It drops iterations for "Other, please specify" options
.drop_other <- function(x, column = "key_adj"){
  out <- dplyr::filter(x, stringr::str_detect((!!rlang::sym(column)), "_text", negate = TRUE))
  out
}


# It classify questions based on the structure of the data
.classify_question <- function(x){
  out <- x %>%
    dplyr::group_by(loop_question) %>%
    dplyr::mutate(equal_n = as.integer(dplyr::n_distinct(non_empty_values) == 1))
  out <- dplyr::mutate(out,
                       type_of_question = dplyr::case_when(equal_n == 1 & n == 1 ~ "select one", # Solo hay una N para esa pregunta y una sola columna
                                                           equal_n == 1 & n > 1 ~ "check all that apply", # Hay una sola N y multiples columnas para esa pregunuta
                                                           equal_n == 0 & n > 1 ~ "other", # Hay multiples N y multiples columnas para esa pregunta
                                                           equal_n == 0 & n == 1 ~ "probably a rank question")) # Hay multiples N per una sola columna para esa pregunta
  out
}


# It drops the responses coming from testers
.drop_tester <- function(.survey, .drop){
  if(is.null(.drop)){
    cat("\n ***Caution*** \n
        I have not deleted testers' responses.")
    .survey
  }
  else{
    .survey <- dplyr::filter(.survey, !(country %in% .drop))
    .survey
  }
}



.create_table_detail <- function(survey, metadata){
  out <- as_tibble(sapply(survey, FUN = function(x) sum(!is.na(x))))
  colnames(out) <- "non_empty_values"
  out <- cbind(metadata, out)
  out <- dplyr::select(out, -c(question_text, loop, question, iteration))

  without_text <- .drop_other(metadata)
  without_text <- dplyr::count(without_text, loop_question)

  out <- dplyr::left_join(out, without_text)

  out <- .classify_question(out)

  out <- dplyr::select(out, -c(n, equal_n))

  out
}



.create_table_resume <- function(survey, metadata){
  out <- .create_table_detail(survey, metadata)

  aux <- as_tibble(sapply(survey, FUN = function(x) n_distinct(x, na.rm = TRUE)))
  colnames(aux) <- "options_per_question_row"

  out <- cbind(out, aux)

  out <- .drop_other(out, column = "key_adj")

  out <- out %>%
    dplyr::group_by(loop_question) %>%
    dplyr::mutate(options_per_question_col = dplyr::n_distinct(key_adj))

  out <- dplyr::mutate(out, options_per_question = dplyr::case_when(type_of_question == "select one" ~ options_per_question_row,
                                                                    type_of_question == "check all that apply" ~ options_per_question_col,
                                                                    type_of_question == "other" ~ options_per_question_col,
                                                                    type_of_question == "probably a rank question" ~ options_per_question_row))

  out <- dplyr::distinct(out, loop_question, .keep_all = TRUE)

  out <- dplyr::select(out, loop_question, type_of_question, non_empty_values, options_per_question)

  out
}

