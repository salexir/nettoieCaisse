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

  # Remapping cols
  map <- unlist(bank_institution[!sapply(bank_institution, is.recursive)])

  map <- merge(x =
                 data.frame(
                   val = map,
                   colmap = names(map), row.names = NULL),
               y = data.frame(val = colnames(fileData)))

  # Recomposing file
  recomposedFile <- data.frame(lapply(map$val, function(x) fileData[[x]]))
  colnames(recomposedFile) <- map$colmap



  # Applying logic to recomposed file

  recomposedFile$internal_currency <- bank_institution$internal_currency

  ## Debit/Credit are coerced to abs value, given that the directionality is
  ## given by context.
  recomposedFile$internal_dr <- harmonise_debit_column(recomposedFile$internal_dr,
                                                       bank_institution$column_settings$dr_col_signed)
  recomposedFile$internal_cr <- harmonise_credit_column(recomposedFile$internal_cr,
                                                        bank_institution$column_settings$cr_col_signed)

  recomposedFile$internal_date <- harmonise_dates(recomposedFile$internal_date)

  # Add new cols
  recomposedFile$internal_bank <- make_sentence_case(bank_name)
  recomposedFile$account_type_1 <- make_sentence_case(account_type_1)
  recomposedFile$account_type_2 <- ifelse(account_type_2 == "cc", "CreditCard", "NonCreditCard")

  recomposedFile$internal <- ifelse(account_type_2 == "cc", "CreditCard", "NonCreditCard")


  recomposedFile$internal_amount <- ifelse(is.na(recomposedFile$internal_cr), recomposedFile$internal_dr, recomposedFile$internal_cr)

  recomposedFile$transaction_type <- ifelse(is.na(recomposedFile$internal_cr), "Debit", "Credit")


  recomposedFile$split_amount <- ifelse(recomposedFile$account_type_1 == "Joint",
                                        recomposedFile$internal_amount/2, recomposedFile$internal_amount)

   recomposedFile$signed_split_amount <- ifelse(recomposedFile$transaction_type == "Debit",
                                          -1*recomposedFile$split_amount,
                                          recomposedFile$split_amount)


   recomposedFile$deletion_flag <- ifelse(grepl(pattern = 'preauth', tolower(recomposedFile$internal_merchant)),
                                   1, 0)

  # Reorder columns to an arbitrary format
  sort_order <- c("internal_date", "internal_bank", "internal_currency", "internal_merchant",
                  "internal_dr", "internal_cr", "internal_runningTot", "account_type_1",
                  "account_type_2", "transaction_type", "internal_amount", "split_amount",
                  "signed_split_amount", "deletion_flag")

  recomposedFile <- recomposedFile[, sort_order]

  recomposedFile


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

  # Check if the readfile name matches any of the allowed fileNames
  flag <- any(sapply(allowed_fileNames, grepl, file))

  stopifnot("Named file doesn't meet aux-bourses-citoyens requirements." = flag > 0)


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

# 3.0 Exchange Rates and such ==================================================





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

