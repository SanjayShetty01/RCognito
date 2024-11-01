#' Initiate Forgot Password Request
#'
#'@import paws
#'
#' @param client_id A character string representing the app client ID.
#' @param username A character string for the user's username.
#'
#' @return Nothing (displays a message on success or raises an error if it fails).
#' @export
forgot_password <- function(client_id, username) {
  tryCatch({

    cognito <- paws::cognitoidentityprovider()

    cognito$forgot_password(ClientId = client_id, Username = username)

    message("Forgot password request sent successfully.")
    invisible(NULL)

  }, error = function(e) {
    stop("Failed to initiate forgot password request: ", e$message)
  })
}
