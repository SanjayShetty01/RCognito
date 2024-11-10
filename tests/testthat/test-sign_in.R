library(testthat)
library(mockery)

testthat::test_that("sign_in function works with mocked AWS Cognito response", {

  mock_cognito <- mockery::mock(list(
    initiate_auth = function(AuthFlow, AuthParameters, ClientId) {

      list(
        AuthenticationResult = list(
          AccessToken = "mock_access_token",
          IdToken = "mock_id_token",
          RefreshToken = "mock_refresh_token"
        )
      )
    }
  ))

  mockery::stub(sign_in, "paws::cognitoidentityprovider", mock_cognito)

  result <- sign_in(
    client_id = "mock_client_id",
    username = "johndoe",
    password = "mock_password123"
  )

  # Assertions to verify the expected behavior
  testthat::expect_type(result, "list")
  testthat::expect_true("AuthenticationResult" %in% names(result))
  testthat::expect_equal(result$AuthenticationResult$AccessToken, "mock_access_token")
  testthat::expect_equal(result$AuthenticationResult$IdToken, "mock_id_token")
  testthat::expect_equal(result$AuthenticationResult$RefreshToken, "mock_refresh_token")
})

testthat::test_that("sign_in function handles failure with mocked
                    AWS Cognito response", {

  mock_cognito <- mockery::mock(list(
    initiate_auth = function(AuthFlow, AuthParameters, ClientId) {
      stop("NotAuthorizedException: Incorrect username or password.")
    }
  ))

  mockery::stub(sign_in, "paws::cognitoidentityprovider", mock_cognito)

  testthat::expect_error(
    sign_in(
      client_id = "mock_client_id",
      username = "wronguser",
      password = "wrong_password"
    ),
    "Failed to sign in: NotAuthorizedException: Incorrect username or password."
  )
})

