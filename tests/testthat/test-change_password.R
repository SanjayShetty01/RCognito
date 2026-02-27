# library(testthat)
library(mockery)

testthat::test_that("change_password succeeds with mocked AWS Cognito response", {
  mock_cognito <- mockery::mock(list(
    change_password = function(PreviousPassword, ProposedPassword, AccessToken) {
      list()
    }
  ))

  mockery::stub(change_password, "paws.security.identity::cognitoidentityprovider", mock_cognito)

  expect_message(
    change_password(
      old_password = "OldPass123!",
      new_password = "NewPass456!",
      token = "mock_access_token"
    ),
    "Password changed successfully."
  )
})

testthat::test_that("change_password handles failure with mocked AWS Cognito response", {
  mock_cognito <- mockery::mock(list(
    change_password = function(PreviousPassword, ProposedPassword, AccessToken) {
      stop("NotAuthorizedException: Incorrect old password.")
    }
  ))

  mockery::stub(change_password, "paws.security.identity::cognitoidentityprovider", mock_cognito)

  expect_error(
    change_password(
      old_password = "WrongPass!",
      new_password = "NewPass456!",
      token = "mock_access_token"
    ),
    "Failed to change password: NotAuthorizedException: Incorrect old password."
  )
})
