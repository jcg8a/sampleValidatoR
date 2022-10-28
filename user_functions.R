
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



















#-------------------------------------------------------------------------------
# User function validate exclusive
#-------------------------------------------------------------------------------

v_exclusive <- function(..., survey = tb_survey){
  # 1. Common validators
  # 2. Input formatting
  input <- ...names()
  # 3. Column selection
  table <- list()
  for (i in 1:length(input)) {
    table[[i]] <- .filter_columns(survey, input[i])
  }
  # 4. Additional validations
  # 5. Main function
  out <- list()
  for (i in 1:length(table)) {
    aux <- list()
    for (j in 1:length(...elt(i))) {
      aux[[j]] <- .validate_exclusive(table[[i]], ...elt(i)[j])
    }
    out[[i]] <- aux

  }
  # 6. Output
  return(out)
}

















#-------------------------------------------------------------------------------
# User function validate rank
#-------------------------------------------------------------------------------

v_rank <- function(..., survey = tb_survey){
  # 1. Common validators
  # 2. Input formatting
  input <- list(...)
  question <- ...names()
  out <- list()
  for (i in 1:length(input)) {
    # 3. Column selection
    aux_q <- .filter_columns(survey, question[i])
    aux_r <- .filter_columns(survey, input[[i]])
    # 4. Additional validations
    # 5. Main function
    out[[i]] <- .validate_most(aux_q, aux_r)
  }
  # 6. Output
  return(out)
}















#-------------------------------------------------------------------------------
# User function validate terminate
#-------------------------------------------------------------------------------
v_terminate <- function(..., survey = tb_survey){
  # 1. Common validators
  # 2. Input formatting
  # 3. Column selection
  aux_terminate <- .filter_columns(survey, ...names()[1])
  aux_q <- .filter_columns_terminate(survey, ...elt(2), ...elt(3))
  # 4. Additional validations
  # 5. Main function
  out <- .validate_terminate(aux_terminate, ...elt(1), aux_q)
  # 6. Output
  return(out)
}













#-------------------------------------------------------------------------------
# User function glance
#-------------------------------------------------------------------------------
glance <- function(question, survey = tb_survey){
  out <- .glance_question(.survey = survey, .pre = question)
  knitr::kable(out)
}













#-------------------------------------------------------------------------------
# User function qualification
#-------------------------------------------------------------------------------
qualification <- function(survey = tb_survey, quali = c("whole Sample", "Complete", "Disqualified", "Bad_Respondent", "Disagree", "Partial", "Robot")){
  tb_qualif <- tibble(qualification = quali,
                      N = rep(0,7), Share = rep(0,7))

  for(i in 1:nrow(tb_qualif)){
    tb_qualif[i,2] <- sum(survey$qualification == tb_qualif$qualification[i])
    tb_qualif[i,3] <- sum(survey$qualification == tb_qualif$qualification[i])/nrow(survey)
  }

  tb_qualif[1,2] <- sum(!is.na(survey$qualification))
  tb_qualif[1,3] <- sum(!is.na(survey$qualification))/nrow(survey)

  knitr::kable(tb_qualif)
}
















#-------------------------------------------------------------------------------
# User function table_detail
#-------------------------------------------------------------------------------
table_detail <- function(survey = tb_survey, meta = metadata){
  out <- .create_table_detail(survey, meta)
  kable(out)
}

















#-------------------------------------------------------------------------------
# User function table_resume
#-------------------------------------------------------------------------------
table_resume <- function(survey = tb_survey, metadata = metadata){
  out <- .create_table_resume(survey, metadata)
  cat("\n **README:** \n
      * Be aware of type_of question = other questions. Explore the table_detail function for a detailed description of the data. \n
      * type_of_question = check all that apply: accounts for the number of columns associated to that loop_question. \n
      * type_of_question =  probably a rank question: I am still testing this guessing. That explains the probably")

  kable(out)
}












#-------------------------------------------------------------------------------
# User function import_metadata
#-------------------------------------------------------------------------------
import_metadata <- function(path_file){

  metadata <- readxl::read_excel(path_file, n_max = 2, col_names = FALSE, .name_repair = "minimal")
  metadata <- as.data.frame(t(metadata))
  metadata <- metadata %>% dplyr::mutate(V3 = dplyr::if_else(str_starts(V1,"D") & stringr::str_starts(V2, "Adjusted"), paste0(V1,"_Adjusted"),V1))

  metadata <- as_tibble(metadata) %>%
    mutate(V3 = str_to_lower(V3))
  colnames(metadata) <- c("key", "question_text", "key_adj")

  metadata <- metadata %>%
    dplyr::mutate(loop = .extract_loop(x = key_adj))
  metadata <- metadata %>%
    dplyr::mutate(iteration = .extract_iteration(x = key_adj, y = loop))
  metadata <- metadata %>%
    dplyr::mutate(question = .extract_question(x = iteration))
  metadata <- metadata %>%
    dplyr::mutate(loop_question = .construct_loop_question(x = question, y = loop))

  metadata
}

















#-------------------------------------------------------------------------------
# User function import_survey
#-------------------------------------------------------------------------------
import_survey <- function(path_file, metadata = metadata, drop_test = c("Spain", "Argentina")){
  tb_survey <- readxl::read_excel(path_file, skip = 0, col_names = FALSE, .name_repair = "minimal", guess_max = 5000)
  tb_survey <- tb_survey[-c(1,2), ]
  colnames(tb_survey) <- t(metadata[,"key_adj"])

  tb_survey <- .drop_tester(.survey = tb_survey, .drop = drop_test)


  tb_survey
}
