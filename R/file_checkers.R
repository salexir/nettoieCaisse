check_fileTypes <- function(file){

  stopifnot("Financial files need to be csv" = tools::file_ext(file) == "csv")

}

check_fileNames <- function(file){

  # small-ish vector of allowable file names
  allowed_fileName_prefixes <- paste0(
    c("personal", "shared", "personal-cc", "shared-cc"), collapse = "|")

(grep(allowed_fileName_prefixes, file))

  stopifnot("File names not in allowable list" = length(grep(allowed_fileName_prefixes, file)) == 1)

}

determine_process_type <- function(file){

  if(grepl('cc', file)){

    if(grepl('personal', file)){

      return('Personal-CreditCard')

    } else{

      return('Joint-CreditCard')

    }

  }else{

    if(grepl('personal', file)){

      return("Personal-NonCreditCard")

    }else{

    return('Joint-NonCreditCard')
  }

  }

}



#' Validate and prepare financial file
#'
#' @param file A filepath to the csv to be processed
#'
#' @return A validated and processed csv file.
#' @export
#'
#' @examples
validate_file <- function(file){

  # Do tests
  check_fileTypes(file)
  check_fileNames(file)
  process_type <- determine_process_type(file)

  col.names <- c("Date", "Merchant", "DR", "CR", "RunningTot", "Note")
  fileData <- utils::read.csv(file = file, header = FALSE, col.names = col.names)


  # Column cleanups
  fileData$Date <- as.Date(fileData$Date, format = '%m/%d/%Y')
  fileData$Merchant <- trimws(fileData$Merchant)
  fileData$Note <- gsub("^$", NA, fileData$Note)


  # Column creation
  fileData$Account_Type <- unlist(strsplit(process_type, "-"))[[1]]
  fileData$Account_Type2 <- unlist(strsplit(process_type, "-"))[[2]]

  fileData$Transaction_Type <- ifelse(is.na(fileData$CR), "Debit", "Credit")

  fileData$Amount <- ifelse(is.na(fileData$CR), fileData$DR, fileData$CR)


  fileData$Split_Amount <- ifelse(fileData$Account_Type == "Joint",
                                   fileData$Amount/2, fileData$Amount)

  fileData$Signed_Split_Amount <- ifelse(fileData$Transaction_Type == "Debit",
                                         -1*fileData$Split_Amount,
                                         fileData$Split_Amount)

  fileData$DeletionFlag <- ifelse(grepl(pattern = 'preauth', tolower(fileData$Merchant)),
                                        1, 0)

  fileData
}
