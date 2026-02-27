library(testthat)
library(mockery)

# --- is_valid_email ---

testthat::test_that("is_valid_email returns TRUE for valid email addresses", {
  expect_true(is_valid_email("user@example.com"))
  expect_true(is_valid_email("john.doe+tag@sub.domain.org"))
  expect_true(is_valid_email("name123@company.co"))
})

testthat::test_that("is_valid_email throws error for invalid email addresses", {
  expect_error(is_valid_email("invalid-email"), "Invalid Email Address")
  expect_error(is_valid_email("missing@"), "Invalid Email Address")
  expect_error(is_valid_email("@nodomain.com"), "Invalid Email Address")
  expect_error(is_valid_email(""), "Invalid Email Address")
  expect_error(is_valid_email("spaces in@email.com"), "Invalid Email Address")
})

# --- is_valid_token ---

testthat::test_that("is_valid_token returns TRUE for valid JWT-shaped tokens", {
  valid_token <- "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.abc123_DEF-456"
  expect_true(is_valid_token(valid_token))
})

testthat::test_that("is_valid_token throws error for invalid tokens", {
  expect_error(is_valid_token("not-a-jwt"), "Invalid Access Token")
  expect_error(is_valid_token(""), "Invalid Access Token")
  expect_error(is_valid_token("only.two"), "Invalid Access Token")
  expect_error(is_valid_token("abc.def.ghi"), "Invalid Access Token")
})

# --- get_user_list_from_cognito ---

testthat::test_that("get_user_list_from_cognito returns usernames and emails", {
  mock_cognito_client <- list(
    list_users = function(UserPoolId, AttributesToGet) {
      list(
        Users = list(
          list(
            Username = "user1",
            Attributes = list(list(Name = "email", Value = "user1@example.com"))
          ),
          list(
            Username = "user2",
            Attributes = list(list(Name = "email", Value = "user2@example.com"))
          )
        )
      )
    }
  )

  result <- get_user_list_from_cognito(
    cognito_client = mock_cognito_client,
    userpool = "mock_userpool_id"
  )

  expect_type(result, "list")
  expect_equal(result$usernames, c("user1", "user2"))
  expect_equal(result$email_id, c("user1@example.com", "user2@example.com"))
})

testthat::test_that("get_user_list_from_cognito handles empty user pool", {
  mock_cognito_client <- list(
    list_users = function(UserPoolId, AttributesToGet) {
      list(Users = list())
    }
  )

  result <- get_user_list_from_cognito(
    cognito_client = mock_cognito_client,
    userpool = "mock_userpool_id"
  )

  expect_type(result, "list")
  expect_length(result$usernames, 0)
  expect_length(result$email_id, 0)
})
