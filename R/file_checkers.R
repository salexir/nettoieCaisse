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



validate_inputFiles <- function(){

  check_fileTypes()

}
