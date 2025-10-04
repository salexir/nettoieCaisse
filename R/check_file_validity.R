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

  # Get bank name
  bank_name <- read_bankName_from_file(filePath)

  # Load bank-model
  bank_institution <- get_bank_model_information(institution = bank_name)[[1]]

  # Read file. This is a full file read as I can't guarantee col number given multiple
  # fin institutions support. Finer selections done purposefully and subsequently.
  fileData <- utils::read.csv(file = filePath,
                              header = bank_institution$file_read_settings$header_present,
                              col.names = bank_institution$file_read_settings$column_position,
                              encoding = bank_institution$file_read_settings$file_encoding)

  fileData


}

read_bankName_from_file <- function(filePath){

  # From ".csv", assert closest fwd slash. Then take characters up to (nt incl.) the first
  # hyphen.

  regmatches(filePath, regexpr("([^/]+?)(?=-[^/]*\\.csv$)", filePath, perl = TRUE))

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
  allowed_fileName_prefixes <- c("personal", "shared", "personal-cc",
                                 "shared-cc")

  # possible name-combinations, flattened
  grid <- expand.grid(bank_institution = bank_institution,
                      allowed_fileName_prefixes = allowed_fileName_prefixes)

  sprintf("%s-%s[[:digit:]]*.csv", grid$bank_institution, grid$allowed_fileName_prefixes)


}
