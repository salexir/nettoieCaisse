#' Validate and prepare aux-bourses-citoyens financial file
#'
#' @param file A filepath to the csv to be processed
#'
#' @returns A validated and augmented dataframe
#' @export
#'
#' @examples
validate_file <- function(filePath){

  # Test file viability
  check_fileTypes(filePath)
  check_fileNames(filePath)

  # Get bank name & other info coded from the filePath
  bank_name <- gather_from_filePath(filePath)[[1]]
  account_type_1 <- gather_from_filePath(filePath)[[2]]
  account_type_2 <- gather_from_filePath(filePath)[[3]]

  # Load bank-model
  bank_institution <- get_bank_model_information(institution = bank_name)[[1]]

  # Read file. This is a full file read as I can't guarantee col number given multiple
  # fin institutions support. Finer selections done purposefully and subsequently.
  fileData <- utils::read.csv(file = filePath,
                              header = bank_institution$file_read_settings$header_present,
                              col.names = bank_institution$file_read_settings$column_position,
                              encoding = bank_institution$file_read_settings$file_encoding,
                              check.names = FALSE)

  fileData


}

### 1.0 File Reading Functions =================================================

gather_from_filePath <- function(filePath){

    information <- regmatches(filePath, regexpr("([^/]+?)(?=\\.csv$)", filePath, perl = TRUE))

    unlist(strsplit(information, "-"))

}

check_fileTypes <- function(file){

  stopifnot("Financial files need to be csv." = tools::file_ext(file) == "csv")

}

check_fileNames <- function(file){

  # First, generate artifacts
  allowed_fileNames <- generate_allowed_fileNames()

  stopifnot("Named file doesn't meet aux-bourses-citoyens requirements " = length(grep(allowed_fileNames, file)) == 1)


}

generate_allowed_fileNames <- function(){

  # Get bank_model information for institution
  bank_institution <- unlist(lapply(set_bank_model(), `[[`, 1), use.names = FALSE)

  # small-ish vector of allowable file names
  allowed_fileName_prefixes <- c("personal-noncc", "shared-noncc", "personal-cc",
                                 "shared-cc")

  # possible name-combinations, flattened
  grid <- expand.grid(bank_institution = bank_institution,
                      allowed_fileName_prefixes = allowed_fileName_prefixes)

  sprintf("%s-%s-[[:digit:]]*.csv", grid$bank_institution, grid$allowed_fileName_prefixes)


}

# 2.0 Column Validating Functions ==============================================

harmonise_dates <- function(column_name){

  as.Date(column_name, tryFormats = c('%m/%d/%Y', '%Y-%m-%d'))

}

harmonise_debit_column <- function(column_name, is_signed){

  if (is_signed){

    vals <- ifelse(column_name <= 0, column_name, NA)

    abs(vals)

  }
  else{

    column_name

  }

}

harmonise_credit_column <- function(column_name, is_signed){

  if (is_signed){

    vals <- ifelse(column_name > 0, column_name, NA)

    abs(vals)

  }
  else{

    column_name
  }

}

# 3.0 Misc =====================================================================

make_sentence_case <- function(string){

  letter1 <- toupper(substr(string, 1, 1))

  return(paste0(letter1, substr(string, 2, nchar(string))))

}

# TESTING UTILS -----

read_test_files <- function(){

  revolut_file <<- validate_file('data-raw/n2-support/Revolut-personal-cc-1.csv')

  td_file <<- validate_file('data-raw/n2-support/TD-personal-cc-1.csv')

  old_process <<- validate_file_old('data-raw/n2-support/TD-personal-cc-1.csv')

  print('done')
}

