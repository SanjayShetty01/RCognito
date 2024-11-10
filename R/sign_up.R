#' Sign up a new user to AWS Cognito
#'
#'@import paws
#'
#' @param client_id A character string representing the app client ID.
#' @param email A character string for the user's email address.
#' @param username A character string for the user's username.
#' @param password A character string for the user's password.
#' @param ... Additional named user attributes for Cognito (e.g., `phone_number`).
#'
#' @return A list with sign-up confirmation details if successful; throws an error otherwise.
#' @export

sign_up_user <- function(client_id, email, username, password, ...) {

  user_attributes <- list(
    list(Name = "email", Value = email),
    ...
  )

  tryCatch({

    cognito <- paws::cognitoidentityprovider()

    sign_up_result <- cognito$sign_up(
      ClientId = client_id,
      Username = username,
      Password = password,
      UserAttributes = user_attributes
    )

    return(sign_up_result)

  }, error = function(e) {
    stop("Failed to sign up: ", e$message)
  })
}
