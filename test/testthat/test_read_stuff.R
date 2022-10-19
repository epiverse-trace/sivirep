
test_that("tests for read_stuff", {

  # test warnings and errors
  msg <- "Must be of type 'character', not 'double'."
  expect_error(read_stuff(123), msg)

  msg <- "Must have length 1, but has length 26."
  expect_error(read_stuff(letters), msg)

  msg <- "Must be of type 'character', not 'NULL'."
  expect_error(read_stuff(NULL), msg)

  msg <- "Must have length 1, but has length 0."
  expect_error(read_stuff(character(0)))

    
  # test functionalities
  path <- getwd()
  expect_identical(path, read_stuff(path))
    
})
