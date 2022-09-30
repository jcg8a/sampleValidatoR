

#-------------------------------------------------------------------------------
# Aux validate other
#-------------------------------------------------------------------------------

# Input: .df should be a data frame with TWO columns in this order:
#               1. The "other, please specify:" option.
#               2. The open ended response.
# Output: an standardized named vector.

.validate_other <- function(.df, .text_other= "Other, please specify:", .autofiller = "-99"){

  aux_other <- mutate(.df, result_col_other = if_else(.df[,1] == .text_other, 1L, 0L, missing = NA_integer_),
                      result_col_oe = if_else(.df[,2] != .autofiller, 1L, 0L, missing = NA_integer_))

  aux_other <- mutate(aux_other, result_col = aux_other[, "result_col_other"] - aux_other[, "result_col_oe"])

  out <- c(sum(aux_other[,"result_col_other"], na.rm = TRUE),
           sum(aux_other[,"result_col_oe"], na.rm = TRUE),
           sum(aux_other[, "result_col"] != 0, na.rm = TRUE),
           sum(aux_other[, "result_col"] == 1, na.rm = TRUE),
           sum(aux_other[, "result_col"] == -1, na.rm = TRUE))

  names(out) <- c("respondent_other", "respondent_oe", "result", "result_forward", "result_backward")

  return(out)
}












#-------------------------------------------------------------------------------
# Aux validate exclusive
#-------------------------------------------------------------------------------

# Input: .df should be a data frame of multiple columns (as many as the target question has)
# Output: an standardized named vector.

.validate_exclusive <- function(.df, .pre_option){

  aux_exclusive <- mutate(.df, result_col_pre = rowSums(.df == .pre_option, na.rm = TRUE),
                          result_col_post = case_when(rowSums(!is.na(.df)) == 0 ~ 0L,
                                                      rowSums(.df != "0", na.rm = TRUE) - rowSums(.df == .pre_option, na.rm = TRUE) > 0 ~ 0L,
                                                      TRUE ~ 1L))

  aux_exclusive[, "result_col"] <- aux_exclusive[, "result_col_pre"] - aux_exclusive[, "result_col_post"]

  out <- c(sum(aux_exclusive[, "result_col_pre"], na.rm = TRUE),
           sum(aux_exclusive[, "result_col_post"], na.rm = TRUE),
           sum(aux_exclusive[, "result_col"] != 0, na.rm = TRUE),
           sum(aux_exclusive[, "result_col"] == 1, na.rm = TRUE),
           sum(aux_exclusive[, "result_col"] == -1, na.rm = TRUE))

  names(out) <- c("respondent_pre", "respondent_post", "result", "result_forward", "result_backward")

  return(out)
}









#-------------------------------------------------------------------------------
# Aux validate rank most
#-------------------------------------------------------------------------------

# Input: Two df: one for check all that apply columns. Other for ranking columns
# Output: an standardized named vector.

.validate_most <- function(.df_pre, .df_post){
  aux_most <- tibble(pre_count = apply(.df_pre, 1, function(x) sum(x != "0")))
  aux_most <- mutate(aux_most, pre_complete = if_else(pre_count > 1, 1L, 0L, missing = NA_integer_),
                     post_count = apply(.df_post, 1, function(x) sum(!is.na(x))),
                     post_complete = if_else(post_count > 0, 1L, NA_integer_, missing = NA_integer_))

  aux_most[ ,"result_col"] <- aux_most[, "pre_complete"] - aux_most[, "post_complete"]

  out <- c(sum(aux_most[, "pre_complete"], na.rm = TRUE),
           sum(aux_most[, "post_complete"], na.rm = TRUE),
           sum(aux_most[, "result_col"] != 0, na.rm = TRUE),
           sum(aux_most[, "result_col"] == 1, na.rm = TRUE),
           sum(aux_most[, "result_col"] == -1, na.rm = TRUE))

  names(out) <- c("respondent_pre", "respondent_post", "result", "result_forward", "result_backward")

  return(out)
}










#-------------------------------------------------------------------------------
# Aux validate terminate
#-------------------------------------------------------------------------------

# Input: Two df: one for the question with the terminate option, called q_terminate.
#                Other for the questions going from the next to q_terminate and the last before demographic variables
#        One vector of options that trigger the terminate option
# Output: an standardized named vector.

.validate_terminate <- function(.df_pre, .pre_option, .df_post){
  aux_terminate <- tibble(result_col_pre = if_else(apply(.df_pre, 1, function(x) sum(x %in% .pre_option)) > 0, 1L, 0L))

  aux_terminate <- mutate(aux_terminate,
                          result_col_post = if_else(apply(.df_post, 1, function(x) sum(!is.na(x))) > 0, 0L, 1L))

  aux_terminate[ ,"result_col"] <- aux_terminate[, "result_col_pre"] - aux_terminate[, "result_col_post"]

  out <- c(sum(aux_terminate[, "result_col_pre"], na.rm = TRUE),
           sum(aux_terminate[, "result_col_post"], na.rm = TRUE),
           sum(aux_terminate[, "result_col"] != 0, na.rm = TRUE),
           sum(aux_terminate[, "result_col"] == 1, na.rm = TRUE),
           sum(aux_terminate[, "result_col"] == -1, na.rm = TRUE))

  names(out) <- c("respondent_pre", "respondent_post", "result", "result_forward", "result_backward")

  return(out)
}










#-------------------------------------------------------------------------------
# Aux validate two way
#-------------------------------------------------------------------------------

# Input: two vectors of integer values
# Output: an standardized named vector.
.validate_two_way <- function(.pre, .post){
  aux_compare <- cbind(.pre, .post)
  colnames(aux_compare) <- c("result_col_pre", "result_col_post")

  result_col <- aux_compare[, "result_col_pre"] - aux_compare[, "result_col_post"]

  out <- c(sum(aux_compare[, "result_col_pre"], na.rm = TRUE),
           sum(aux_compare[, "result_col_post"], na.rm = TRUE),
           sum(result_col != 0, na.rm = TRUE),
           sum(result_col == 1, na.rm = TRUE),
           sum(result_col == -1, na.rm = TRUE))

  names(out) <- c("respondent_pre", "respondent_post", "result", "result_forward", "result_backward")

  return(out)
}

