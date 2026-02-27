library(testthat)
library(mockery)

testthat::test_that("sign_up_user function works with mocked AWS Cognito response", {
  mock_cognito <- mockery::mock(list(
    sign_up = function(ClientId, Username, Password, UserAttributes) {
      list(
        UserConfirmed = FALSE,
        CodeDeliveryDetails = list(
          AttributeName = "email",
          DeliveryMedium = "EMAIL",
          Destination = "j***@example.com"
        )
      )
    }
))

mockery::stub(sign_up_user, "paws::cognitoidentityprovider", mock_cognito)

result <- sign_up_user(
  client_id = "mock_client_id",
  email = "johndoe@example.com",
  username = "johndoe",
  password = "mock_password123"
)

testthat::expect_type(result, "list")
testthat::expect_false(result$UserConfirmed)
testthat::expect_equal(result$CodeDeliveryDetails$AttributeName, "email")
testthat::expect_equal(result$CodeDeliveryDetails$DeliveryMedium, "EMAIL")
})

testthat::test_that("sign_up_user handles failure with mocked AWS Cognito response", {
  mock_cognito <- mockery::mock(list(
    sign_up = function(ClientId, Username, Password, UserAttributes) {
      stop("UsernameExistsException: An account with the given email already exists.")
    }
  ))

  mockery::stub(sign_up_user, "paws::cognitoidentityprovider", mock_cognito)

  testthat::expect_error(
    sign_up_user(
      client_id = "mock_client_id",
      email = "existing@example.com",
      username = "existinguser",
      password = "mock_password123"
    ),
    "Failed to sign up: UsernameExistsException: An account with the given email already exists."
  )
})

