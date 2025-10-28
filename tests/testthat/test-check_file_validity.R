# 1.0 File Read Validation  ====================================================

test_that("importing non-csv fin file throws error",
          {
            invalid_file = withr::local_tempfile(fileext = ".doc")
            expect_error(check_fileTypes(file = invalid_file, class = "error"))

          }
)

test_that("generated file names have the elements",{
          expect_match(generate_allowed_fileNames(),
                          paste0(c("personal", "shared", "personal-cc", "shared-cc"),
                                 collapse = "|")) })

test_that("file type passed have csv as file extension. This is only checking for file name, not a formal extension test. ",
          {
            expect_silent(check_fileTypes("helloworld.csv"))
          })


## 1.1 Well formed names =======================================================

test_that("well formed file names are passed silently",
          {expect_silent(check_fileNames(
            "TD-personal-noncc-1.csv"
          ))})

test_that("well formed file names are passed silently",
          {expect_silent(check_fileNames(
            "Revolut-shared-cc-13.csv"
          ))})

test_that("mal formed file names are raised",
          {expect_error(check_fileNames(
            "Revolut-cc-13.csv"
          ))})

test_that("mal formed file names are raised",
          {expect_error(check_fileNames(
            "Revolut-cc-personal-2.csv"
          ))})

test_that("mal formed file names are raised",
          {expect_error(check_fileNames(
            "TDA-personal-2"
          ))})

test_that("mal formed file names are raised",
          {expect_error(check_fileNames(
            "TD-personal-cc.csv"
          ))})

## 1.2 filePath Info extraction ================================================

test_that("ensure return element of 3, for bank, account type, cc/non-cc",
          {expect_length(gather_from_filePath("TD-personal-cc.csv"), 3)})


test_that("ensure return element is unnested",
          {expect_false(is.recursive(gather_from_filePath("TD-personal-cc.csv")))})

test_that("ensure return element is in a particular order: bank, a/c type, cc/non-cc",
          {expect_identical(gather_from_filePath("TD-personal-cc.csv"), c("TD", "personal", "cc"))})

# 2.0 Column Validating Functions ==============================================

test_that("harmonise_dates() coerces correctly; YYYY-MM-DD",
          {
            dates <- c("2022-12-01", "2022-10-15")

            expect_equal(as.character(harmonise_dates(dates)),
                         c("2022-12-01", "2022-10-15"))

          })

test_that("harmonise_dates() coerces correctly; MM/DD/YYYY",
          {
            dates <- c("03/01/2014", "12/23/2025")

            expect_equal(as.character(harmonise_dates(dates)),
                         c("2014-03-01", "2025-12-23"))

          })

test_that("harmonise debit column always returns positive values, given signed col",
          {
            expect_setequal(harmonise_debit_column(c(30, -12, 0), is_signed = TRUE),
                            c(NA, 12, 0))
          })


test_that("harmonise debit column always returns positive values, given signed col",
          {
            expect_setequal(harmonise_debit_column(c(30, 12, 0), is_signed = FALSE),
                            c(30, 12, 0))
          })


test_that("harmonise credit column always returns positive values, given signed col",
          {
            expect_setequal(harmonise_credit_column(c(30, -12, 0), is_signed = TRUE),
                            c(30, NA, NA))
          })


test_that("harmonise debit column always returns positive values, given signed col",
          {
            expect_setequal(harmonise_credit_column(c(30, 12, 0), is_signed = FALSE),
                            c(30, 12, 0))
          })

# 3.0 Test main validator via example files ====================================

## 3.1 Institution: TD =========================================================

### 3.1.1 File Structure =======================================================

test_that("Colnames map after process",
          {
            expect_setequal(
              colnames(validate_file(test_path('fixtures/mock-files/TD-personal-cc-1.csv'))),
              c("internal_date", "internal_bank", "internal_currency", "internal_merchant",
                "internal_dr", "internal_cr", "internal_runningTot",
                "account_type_1", "account_type_2", "transaction_type",
                "internal_amount",
                "CAD_amount", "CAD_split_amount", "CAD_signed_split_amount",
                "GBP_amount", "GBP_split_amount", "GBP_signed_split_amount",
                "deletion_flag")
              )
            })


test_that("File well-formed: no unusual NAs present in core-cols",
          {
            testfile <- validate_file(test_path('fixtures/mock-files/TD-personal-cc-1.csv'))

            analysis_of_na <- sapply(testfile, function(x) sum(is.na(x)))

            core_cols <- c("internal_date",
                           "internal_bank",
                           "internal_merchant",
                           "account_type_1",
                           "account_type_2",
                           "transaction_type",
                           "internal_amount",
                           "CAD_amount", "CAD_split_amount", "CAD_signed_split_amount",
                           "GBP_amount", "GBP_split_amount", "GBP_signed_split_amount",
                           "deletion_flag")

            expect_equal(sum(analysis_of_na[core_cols]), 0)

          })

## 3.2 Institution: Revolut ====================================================

test_that("Colnames map after process",
          {
            expect_setequal(
              colnames(validate_file(test_path('fixtures/mock-files/Revolut-personal-noncc-1.csv'))),
              c("internal_date", "internal_bank", "internal_currency", "internal_merchant",
                "internal_dr", "internal_cr", "internal_runningTot",
                "account_type_1", "account_type_2", "transaction_type",
                "internal_amount",
                "CAD_amount", "CAD_split_amount", "CAD_signed_split_amount",
                "GBP_amount", "GBP_split_amount", "GBP_signed_split_amount",
                "deletion_flag")
            )
          })



test_that("File well-formed: no unusual NAs present in core-cols",
          {
            testfile <- validate_file(test_path('fixtures/mock-files/Revolut-personal-noncc-1.csv'))

            analysis_of_na <- sapply(testfile, function(x) sum(is.na(x)))

            core_cols <- c("internal_date",
                           "internal_bank",
                           "internal_merchant",
                           "account_type_1",
                           "account_type_2",
                           "transaction_type",
                           "internal_amount",
                           "CAD_amount", "CAD_split_amount", "CAD_signed_split_amount",
                           "GBP_amount", "GBP_split_amount", "GBP_signed_split_amount",
                           "deletion_flag")

            expect_equal(sum(analysis_of_na[core_cols]), 0)

          })



# 4.0 Misc =====================================================================

test_that("make_sentence_case returns... sentence case",
          {
            expect_equal(make_sentence_case("whaddup"), "Whaddup")
          })

test_that("make_sentence_case returns char type",
          {
          expect_type(make_sentence_case("whaddup"), type = "character")
            }
          )

test_that("make_sentence_case returns obj of length 1",
          {
            expect_length(make_sentence_case("whaddup"), 1)
          }
)
