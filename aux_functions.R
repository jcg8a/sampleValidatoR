

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
