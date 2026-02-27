library(testthat)
library(mockery)

testthat::test_that("forgot_password succeeds with mocked AWS Cognito response", {
  mock_cognito <- mockery::mock(list(
    forgot_password = function(ClientId, Username) {
      list(
        CodeDeliveryDetails = list(
          AttributeName = "email",
          DeliveryMedium = "EMAIL",
          Destination = "j***@example.com"
        )
      )
    }
  ))

  mockery::stub(forgot_password, "paws.security.identity::cognitoidentityprovider", mock_cognito)

  expect_message(
    forgot_password(
      client_id = "mock_client_id",
      username = "johndoe"
    ),
    "Forgot password request sent successfully."
  )
})

testthat::test_that("forgot_password handles failure with mocked AWS Cognito response", {
  mock_cognito <- mockery::mock(list(
    forgot_password = function(ClientId, Username) {
      stop("UserNotFoundException: User does not exist.")
    }
  ))

  mockery::stub(forgot_password, "paws.security.identity::cognitoidentityprovider", mock_cognito)

  expect_error(
    forgot_password(
      client_id = "mock_client_id",
      username = "nonexistentuser"
    ),
    "Failed to initiate forgot password request: UserNotFoundException: User does not exist."
  )
})
