test_that("internal column representations from csvs imported are what are expected",
  {expect_setequal(
    object = unique(unlist(lapply(set_bank_model(), function(x) names(x)))),
    expected = c("internal_bank_name",
                 "internal_date",
                 "internal_merchant",
                 "internal_dr",
                 "internal_cr",
                 "internal_currency",
                 "internal_runningTot",
                 "file_read_settings",
                 "column_settings"))})


test_that("List of bank model variables returns something",
          {
            expect_true(length(get_bank_model_variables())>0)
          })

test_that("get_bank_model_information returns list",
          {
            expect_type(get_bank_model_information(), "list")

          }
          )
