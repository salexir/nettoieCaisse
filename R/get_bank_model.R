set_bank_model <- function(){

  bank_td <-
    list(
      internal_bank_name = "TD",
      internal_date = "Date",
      internal_merchant = "Merchant",
      internal_dr = "DR",
      internal_cr = "CR",
      internal_currency = "CAD",
      internal_runningTot = "RunningTot",
      file_read_settings =
        list(
          header_present = FALSE,
          column_position = c("Date", "Merchant", "DR", "CR", "RunningTot", "Note"),
          file_encoding = "UTF-8"
      ),
      column_settings =
        list(
          dr_col_signed = FALSE,
          cr_col_signed = FALSE
        )
      )

  bank_revolut <-
    list(
      internal_bank_name = "Revolut",
      internal_date = "Completed Date",
      internal_merchant = "Description",
      internal_dr = "Amount",
      internal_cr = "Amount",
      internal_currency = "GBP",
      internal_runningTot = "Balance",
      file_read_settings =
        list(
          header_present = TRUE,
          column_position = c("Type", "Product", "Started Date", "Completed Date",
                              "Description", "Amount", "Fee", "Currency",
                              "State", "Balance"),
          file_encoding = "UTF-8"
        ),
      column_settings =
        list(
          dr_col_signed = TRUE,
          cr_col_signed = TRUE
        )
    )


  list(bank_td = bank_td,
       bank_revolut = bank_revolut)

}

get_bank_model_variables <- function(){

  names(set_bank_model()[[1]])

}

get_bank_model_information <- function(variable = NULL, institution = NULL){

  bank_model <- set_bank_model()

  if (!is.null(institution)){

    # Cut to size if institution is specified
    int_bank_name <- vapply(bank_model, FUN = `[[`, "internal_bank_name", FUN.VALUE = "character")
    bank_model <- bank_model[names(int_bank_name[int_bank_name == institution])]

  }

  if (is.null(variable)){

    variable = get_bank_model_variables()

  }

  lapply(bank_model, '[', variable)

}
