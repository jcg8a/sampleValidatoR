
binarize <- function(..., survey = tb_survey){
  # 1. Common validators
  # Formatting the input
  input <- list(...)
  # 2. Column selection
  tables <- list()
  for (i in 1:length(input)) {
    tables[[i]] <- .filter_columns(survey, input[[i]])
  }
  # 3. Additional validations
  # 4. Main function
  # 5. Output
  return(tables)
}
