test_that("zero_check") {
  zero_check(0, quit = FALSE)
  zero_check(1, quit = FALSE)  
  zero_check(TRUE, quit = FALSE)
  zero_check(FALSE, quit = FALSE)
  zero_check(NULL, quit = FALSE, fail_on_null = TRUE) ## should fail
  zero_check(NULL, quit = FALSE, fail_on_null = FALSE) ## should not fail
}


