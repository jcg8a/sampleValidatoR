

# -------------------------------------------------------------------------------
# Dealing with column name structure
# -------------------------------------------------------------------------------

# Regular column name structure follows this pattern: ##_ALPHANUM_##
# The first two ## show the loop ranging from 1 to 99
# The last two ## show the iteration ranging from 01 to 00
# ALPHANUM shows the name of the question. An alphanumeric value

# Extracts the loop number from the column name
.extract_loop <- function(x, pattern = "(?<![:alnum:]|_)[0-9](?=_[:alpha:])"){
  stringr::str_extract(x, pattern)
}

# Extracts the iteration number from the column name
.extract_iteration <-  function(x, y, pattern = "(?<=_).*"){
  dplyr::if_else(is.na(y), x, stringr::str_extract(x, pattern))
}

# Extracts the name of the question from the column name
.extract_question <- function(x, pattern = "(_[0-9][0-9])|(_[0-9])"){
  stringr::str_replace(stringr::str_replace(x, "_text", ""), pattern, "")
}

# Construct a loop_question name with a ##_ALPHANUM structure. It drops the iteration
.construct_loop_question <- function(x, y){
  dplyr::if_else(is.na(y), x, paste0(y, "_", x))
}









# -------------------------------------------------------------------------------
# Filtering columns to research
# -------------------------------------------------------------------------------

.filter_columns <- function(.df, .loop_question){
  arg_loop <- paste0("^", .loop_question)
  arg_check_all <- paste0(.loop_question, "[_]")
  arg_select_one <- paste0(.loop_question, "$")
  out <- dplyr::select(.df, grep(arg_check_all, colnames (.df)) | grep(arg_select_one, colnames(.df)))
  out <- dplyr::select(out, grep(arg_loop, colnames(out)))

  out
}









# -------------------------------------------------------------------------------
# Not in function
# -------------------------------------------------------------------------------
`%notin%` <- Negate(`%in%`)









# -------------------------------------------------------------------------------
# Labeling options equal to the given parameter
# -------------------------------------------------------------------------------
.label_question_in <- function(.df, .pre_option){
  out <- if_else(rowSums(apply(.df, 2, function(x) x %in% .pre_option)) > 0, 1L, 0L)
}











# -------------------------------------------------------------------------------
# Labeling options not equal to the given parameter
# -------------------------------------------------------------------------------
.label_question_notin <- function(.df, .pre_option){
  out <- if_else(rowSums(apply(.df, 2, function(x) x %in% .pre_option)) == 0, 1L, 0L)
}










# -------------------------------------------------------------------------------
# Binarizing questions when they meet a condition
# -------------------------------------------------------------------------------
.binarize_question <- function(.df, .options){
  out <- if_else(rowSums(!is.na(.df)) == 0,
                 NA_integer_,
                 as.integer(rowSums(sapply(.df, `%in%`, .options), na.rm = TRUE)))
  return(out)
}



.join_binaries <- function(list_questions, list_options){
  aux <- data.frame(id = seq(1:nrow(list_questions[[1]])))
  for(i in 1:length(list_questions)){
    aux_ind <- .binarize_question(.df = list_questions[[i]], .options = list_options[[i]])
    aux <- cbind(aux, aux_ind)
  }
  aux$id <- NULL
  out <- if_else(rowSums(!is.na(aux)) == 0,
                 NA_integer_,
                 if_else(rowSums(aux, na.rm = TRUE) > 0, 1L, 0L))
  return(out)
}






# -------------------------------------------------------------------------------
# Filtering columns to research for validate_other
# -------------------------------------------------------------------------------
.filter_columns_other <- function(.df, .question, .other){
  out <- list()
  aux <- .filter_columns(.df, .question)

  column_index <- unique(which(aux == .other, arr.ind = TRUE)[, 2])

  for (i in 1:length(column_index)) {
    other_column <- colnames(aux[,column_index[i]])

    out[[i]] <- select(aux, other_column, starts_with(other_column) & ends_with("_text"))
  }
  return(out)
}







# -------------------------------------------------------------------------------
# Filtering columns to research for v_terminate
# -------------------------------------------------------------------------------
.filter_columns_terminate <- function(.survey, .q_first, .q_last){
  col_names <- colnames(.survey)
  index_q_first <- min(stringr::str_which(col_names, .q_first))
  index_q_last <- max(stringr::str_which(col_names, .q_last))

  out <- select(.survey, col_names[index_q_first]:col_names[index_q_last])
  return(out)
}














# -------------------------------------------------------------------------------
# List options for the given question
# -------------------------------------------------------------------------------
.glance_question <- function(.survey, .pre){
  aux <- .filter_columns(.df = .survey, .loop_question = .pre)
  # print("filter variables")
  aux <- select(aux, -stringr::str_which(colnames(aux), "_text"))
  # print("drop _text columns")

  if(ncol(aux) == 1){
    out <- aux %>%
      group_by((!!rlang::sym(.pre))) %>%
      summarise(N = n())
    # print("Summarise N")

    out <- filter(out, !is.na(out[, 1]))
    # print(out)
    return(out)
  }

  if(ncol(aux) > 1){
    out <- tibble()
    for(i in 1:ncol(aux)){
      aux_table <- aux %>%
        group_by(aux[,i]) %>%
        summarise(N = n())

      colnames(aux_table) <- c("question", "N")
      aux_table$question <- as.character(aux_table$question)

      out <- bind_rows(out, aux_table)
    }

    out <- filter(out, !is.na(out[,"question"]))
    out <- filter(out, out[, "question"] != 0)

    colnames(out) <- c(.pre, "N")
    return(out)
  }
}
