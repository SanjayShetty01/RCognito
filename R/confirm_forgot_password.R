#' Confirm Forgot Password
#'
#' This function confirms the forgot password process by setting a new password for a user.
#'
#'@import paws
#'
#' @param client_id A character string representing the app client ID.
#' @param username A character string for the user's username.
#' @param confirmation_code A character string for the confirmation code sent to the user.
#' @param new_password A character string for the user's new password.
#'
#' @return Nothing (displays a message on success or raises an error if it fails).
#' @export

confirm_forgot_password <- function(client_id, username, confirmation_code, new_password) {
  tryCatch({

    cognito <- paws::cognitoidentityprovider()

    cognito$confirm_forgot_password(
      ClientId = client_id,
      Username = username,
      ConfirmationCode = confirmation_code,
      Password = new_password
    )


    message("New password set successfully.")
    invisible(NULL)

  }, error = function(e) {
    stop("Failed to set new password: ", e$message)
  })
}
