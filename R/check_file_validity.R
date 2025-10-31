#' Validate and prepare aux-bourses-citoyens financial file
#'
#' @param filePath A filepath to the csv to be processed
#'
#' @returns A validated and augmented dataframe
#' @export
#'
#' @examples
#' print("helloworld")
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

  recomposedFile <- convert_to_cad_from_gbp(recomposedFile)
  recomposedFile <- convert_to_gbp_from_cad(recomposedFile)

  recomposedFile$deletion_flag <- ifelse(grepl(pattern = 'preauth', tolower(recomposedFile$internal_merchant)),
                                         1, 0)

  # Reorder columns to an arbitrary format
  sort_order <- c("internal_date", "internal_bank", "internal_currency", "internal_merchant",
                  "internal_dr", "internal_cr", "internal_runningTot",
                  "account_type_1", "account_type_2", "transaction_type",
                  "internal_amount",
                  "CAD_amount", "CAD_split_amount", "CAD_signed_split_amount",
                  "GBP_amount", "GBP_split_amount", "GBP_signed_split_amount",
                  "deletion_flag")

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
  allowed_fileName_prefixes <- c("personal-noncc", "joint-noncc", "personal-cc",
                                 "joint-cc")

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

# 3.0 Fin Calculations, Exchange Rates and such ================================

calculate_using_specific_currency <- function(return_currency, FXQuote){

  fin_name <- sprintf("%s_amount", return_currency)
  split_name <- sprintf("%s_split_amount", return_currency)
  signed_split_name <- sprintf("%s_signed_split_amount", return_currency)



  function(recomposedFile){

    fx_information <- get_fx_information(FXQuote = FXQuote)


    recomposedFile <- merge(x = recomposedFile,
                            y = fx_information,
                            by.x = "internal_date",
                            by.y = "Date",
                            all.x = TRUE)

    ## Determine conversion factor
    fx_conversion_factor <- ifelse(recomposedFile$internal_currency[[1]] == substr(return_currency,1,3),
                                   1, recomposedFile$Avg_imputed)


    recomposedFile[[fin_name]] <- round(convert_amount(fin_col = recomposedFile$internal_amount,
                                                 fx_conversion_factor = fx_conversion_factor),2)

    recomposedFile[[split_name]] <-
      round(split_amount(account_type = recomposedFile$account_type_1,
                   fin_col = recomposedFile[[fin_name]]), 2)


    recomposedFile[[signed_split_name]] <-
      round(sign_amount(transaction_type = recomposedFile$transaction_type,
                  split_col = recomposedFile[[split_name]]),2)

    # Cleanup
    recomposedFile$FXQuote <- NULL
    recomposedFile$Date <- NULL
    recomposedFile$Avg_imputed <- NULL


    recomposedFile


  }

}

convert_to_gbp_from_cad <- calculate_using_specific_currency(return_currency = "GBP", FXQuote = "CAD/GBP")
convert_to_cad_from_gbp <- calculate_using_specific_currency(return_currency = "CAD", FXQuote = "GBP/CAD")

convert_amount <- function(fin_col, fx_conversion_factor = 1){
  # This applies fx

  fin_col*fx_conversion_factor

}


split_amount <- function(account_type, fin_col){
  # This doesn't apply fx; expect an already fx'ed col

  ifelse(account_type == "Joint",
         fin_col/2, fin_col)

}

sign_amount <- function(transaction_type, split_col){
  # This doesn't apply fx; expect an already fx'ed col

  ifelse(transaction_type == "Debit",
         -1*split_col, split_col)

}

get_fx_information <- function(FXQuote = "GBP/CAD"){


 ## First. let's get the historical stuff.
 hist <- cad_gbp_viceversa_quotes


 ## Next, let's supplement with the most recent data
  current <- as.data.frame(quantmod::getFX(FXQuote, auto.assign = FALSE))

  ## Reorganise:
  current <- data.frame(Date = row.names(current),
                         Avg_imputed = current[,1])

  current$FXQuote <- FXQuote

  if (FXQuote %in% c("GBP/CAD", "CAD/GBP")){


    ## Take only net new for GBP/CAD
    diff <- setdiff(current$Date, hist$Date)

    current <- current[!current$Date %in% diff, ]

    list(hist, current)

    ## Collate

    full_dat <- rbind(hist[, c("Date", "Avg_imputed", "FXQuote")], current)

    ## Send only relevant

    full_dat[full_dat$FXQuote == FXQuote,]

  }else{

    current

  }

}

# 4.0 Misc =====================================================================

make_sentence_case <- function(string){

  letter1 <- toupper(substr(string, 1, 1))

  return(paste0(letter1, substr(string, 2, nchar(string))))

}

# 5.0 TESTING UTILS ============================================================

## Comment out when building.
#'
#' read_test_files <- function(){
#'
#'   revolut_file <<- validate_file('untrack/n2-support/Revolut-personal-cc-1.csv')
#'
#'   td_file <<- validate_file('untrack/n2-support/TD-personal-cc-1.csv')
#'
#'   old_process_td_file <<- validate_file_old('untrack/n2-support/TD-personal-cc-1.csv')
#'
#'   print('done')
#' }
#'
#' #' Validate and prepare financial file
#' #'
#' validate_file_old <- function(file){
#'
#'   # Do tests
#'   check_fileTypes(file)
#'   check_fileNames(file)
#'   process_type <- determine_process_type(file)
#'
#'   col.names <- c("Date", "Merchant", "DR", "CR", "RunningTot", "Note")
#'   fileData <- utils::read.csv(file = file, header = FALSE, col.names = col.names)
#'
#'
#'   # Column cleanups
#'   fileData$Date <- as.Date(fileData$Date, tryFormats = c('%m/%d/%Y', '%Y-%m-%d'))
#'   fileData$Merchant <- trimws(fileData$Merchant)
#'   fileData$Note <- gsub("^$", NA, fileData$Note)
#'
#'
#'   # Column creation
#'   fileData$Account_Type <- unlist(strsplit(process_type, "-"))[[1]]
#'   fileData$Account_Type2 <- unlist(strsplit(process_type, "-"))[[2]]
#'
#'   fileData$Transaction_Type <- ifelse(is.na(fileData$CR), "Debit", "Credit")
#'
#'   fileData$Amount <- ifelse(is.na(fileData$CR), fileData$DR, fileData$CR)
#'
#'
#'   fileData$Split_Amount <- ifelse(fileData$Account_Type == "Joint",
#'                                   fileData$Amount/2, fileData$Amount)
#'
#'   fileData$Signed_Split_Amount <- ifelse(fileData$Transaction_Type == "Debit",
#'                                          -1*fileData$Split_Amount,
#'                                          fileData$Split_Amount)
#'
#'   fileData$DeletionFlag <- ifelse(grepl(pattern = 'preauth', tolower(fileData$Merchant)),
#'                                   1, 0)
#'
#'   fileData
#' }
#' determine_process_type <- function(file){
#'
#'   if(grepl('cc', file)){
#'
#'     if(grepl('personal', file)){
#'
#'       return('Personal-CreditCard')
#'
#'     } else{
#'
#'       return('Joint-CreditCard')
#'
#'     }
#'
#'   }else{
#'
#'     if(grepl('personal', file)){
#'
#'       return("Personal-NonCreditCard")
#'
#'     }else{
#'
#'       return('Joint-NonCreditCard')
#'     }
#'
#'   }
#'
#' }

