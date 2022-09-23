

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
