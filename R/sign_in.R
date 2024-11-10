#' Sign in to AWS Cognito
#'
#'@import paws
#'
#' @param client_id A character string representing the app client ID.
#' @param username A character string for the user's username.
#' @param password A character string for the user's password.
#' @param authflow A character string specifying the authentication flow (default: "USER_PASSWORD_AUTH").
#'
#' @return A list with user information and tokens if successful; throws an error otherwise.
#' @export

sign_in <- function(client_id, username, password,
                    authflow = "USER_PASSWORD_AUTH") {
  tryCatch({

    cognito <- paws::cognitoidentityprovider()

    auth_parameter <- list(USERNAME = username, PASSWORD = password)

    user_info <- cognito$initiate_auth(
      AuthFlow = authflow,
      AuthParameters = auth_parameter,
      ClientId = client_id
    )

    return(user_info)

  }, error = function(e) {
    stop("Failed to sign in: ", e$message)
  })
}
