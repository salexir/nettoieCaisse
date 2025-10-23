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
# 3.0 Misc =====================================================================

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

### SUPERSEEDED
#
# test_that("Assert bankname read from file is not mangled",
#           {
#             expect_equal(read_bankName_from_file("data/sdi/3940/TD-shared.csv"), expected = "TD")
#
#           })
#
# test_that("Assert bankname read from file is not mangled",
#           {
#             expect_equal(read_bankName_from_file("data/sdi/3940/Revolut-shared2.csv"), expected = "Revolut")
#
#           })
#
# test_that("Assert bankname read from file is not mangled",
#           {
#             expect_equal(read_bankName_from_file("Revolut-shared2.csv"), expected = "Revolut")
#
#           })
