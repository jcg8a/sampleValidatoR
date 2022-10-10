
binarize <- function(..., survey = tb_survey){
  # 1. Common validators
  # 2. Input formatting
  q_input <- ...names()
  input <- list(...)
  # 3. Column selection
  tables <- list()
  for (i in 1:length(q_input)) {
    tables[[i]] <- .filter_columns(survey, q_input[i])
  }
  # 4. Additional validations
  # 5. Main function
  out <- .join_binaries(tables, input)
  # 5. Output
  return(out)
}











#-------------------------------------------------------------------------------
# User function validate other
#-------------------------------------------------------------------------------

v_other <- function(..., survey = tb_survey, text_other = "Other, please specify:", autofiller = "-99"){
  # 1. Common validators
  # 2. Input formatting
  input <- list(...)
  # 3. Column selection
  tables <- list()
  for(i in 1:length(input)){
    tables[[i]] <- .filter_columns_other(survey, input[[i]], .other = text_other)
  }
  # 4. Additional validations
  # 5. Main function
  for (i in 1:length(tables)) {
    for (j in 1:length(tables[[i]])) {
      tables[[i]][[j]] <- .validate_other(.df = tables[[i]][[j]], .text_other = text_other, .autofiller = autofiller)
    }
  }
  # 6. Output
  return(tables)
}
