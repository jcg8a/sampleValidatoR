
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
