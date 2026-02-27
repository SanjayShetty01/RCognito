library(testthat)
library(mockery)

testthat::test_that("confirm_forgot_password succeeds with mocked AWS Cognito response", {
  mock_cognito <- mockery::mock(list(
    confirm_forgot_password = function(ClientId, Username,
                                       ConfirmationCode, Password) {
      list()
    }
  ))

  mockery::stub(confirm_forgot_password,
                "paws::cognitoidentityprovider",
                mock_cognito)

  expect_message(
    confirm_forgot_password(
      client_id = "mock_client_id",
      username = "johndoe",
      confirmation_code = "123456",
      new_password = "NewSecurePass789!"
    ),
    "New password set successfully."
  )
})

testthat::test_that("confirm_forgot_password handles failure with mocked
                    AWS Cognito response", {
  mock_cognito <- mockery::mock(list(
    confirm_forgot_password = function(ClientId, Username,
                                       ConfirmationCode, Password) {
      stop("CodeMismatchException: Invalid verification code provided.")
    }
  ))

  mockery::stub(confirm_forgot_password,
                "paws::cognitoidentityprovider",
                mock_cognito)

  expect_error(
    confirm_forgot_password(
      client_id = "mock_client_id",
      username = "johndoe",
      confirmation_code = "000000",
      new_password = "NewSecurePass789!"
    ),
    "Failed to set new password: CodeMismatchException: Invalid verification code provided."
  )
})
