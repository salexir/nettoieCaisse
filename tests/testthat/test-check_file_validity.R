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


test_that("Assert bankname read from file is not mangled",
          {
            expect_equal(read_bankName_from_file("data/sdi/3940/TD-shared.csv"), expected = "TD")

          })

test_that("Assert bankname read from file is not mangled",
          {
            expect_equal(read_bankName_from_file("data/sdi/3940/Revolut-shared2.csv"), expected = "Revolut")

          })

test_that("Assert bankname read from file is not mangled",
          {
            expect_equal(read_bankName_from_file("Revolut-shared2.csv"), expected = "Revolut")

          })
