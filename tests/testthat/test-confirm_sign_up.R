library(testthat)
library(mockery)

testthat::test_that("confirm_sign_up_user function works with mocked AWS
                    Cognito response", {

  mock_user_list <- mockery::mock(
    list(usernames = c("existinguser", "johndoe")),
    cycle = TRUE)

  mockery::stub(confirm_sign_up_user,
                "get_user_list_from_cognito",
                mock_user_list)

  mock_cognito <- mockery::mock(list(
    confirm_sign_up = function(ClientId, Username, ConfirmationCode) {
      list()
    }
  ), cycle = TRUE)

  mockery::stub(confirm_sign_up_user,
                "paws::cognitoidentityprovider",
                mock_cognito)

 testthat::expect_message(
    confirm_sign_up_user(
      client_id = "mock_client_id",
      userpool = "mock_userpool",
      username = "johndoe",
      verification_code = "123456"
    )
  )


  testthat::expect_error(
    confirm_sign_up_user(
      client_id = "mock_client_id",
      userpool = "mock_userpool",
      username = "nonexistentuser",
      verification_code = "123456"
    ),
    "User not found, Please enter correct username"
  )

})
