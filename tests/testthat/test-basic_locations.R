test_that("basic locations works", {
  skip_on_cran()

  # valid location
  location1 <- cqc_location("1-1428643301")
  expect_true(is.list(location1))

  # valid location inspection area
  insp_loc1 <- cqc_location_inspection_area("1-122710325")
  expect_true(is.list(insp_loc1))

  # Invalid location returns error data frame
  error_result <- cqc_location("blaah")
  expect_s3_class(error_result, "data.frame")
  expect_true(all(c("error_message", "url") %in% colnames(error_result)))
  expect_type(error_result$error_message, "character")
  expect_gt(nchar(error_result$error_message[1]), 0)

  options("cqc.subscription.key" = NULL) # Clear the option after the test
})
