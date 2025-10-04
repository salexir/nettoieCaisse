test_that("generate_allowed_filenames() generates a vector of results",
          {expect_vector(generate_allowed_fileNames())
            }
          )

test_that("generate_allowed_filenames() generates filenames with support for digits in them",
          {expect_match(generate_allowed_fileNames(), regexp = '\\[\\[:digit:\\]\\]')})
