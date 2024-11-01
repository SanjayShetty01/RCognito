#' Change User Password
#'
#'@import paws
#'
#' @param old_password A character string representing the user's current password.
#' @param new_password A character string representing the user's new password.
#' @param token A character string representing the user's access token.
#'
#' @return A message indicating the success of the password change, or an error if it fails.
#' @export

change_password <- function(old_password, new_password, token) {

  tryCatch({
    cognito <-  paws::cognitoidentityprovider()

    cognito$change_password(PreviousPassword = old_password,
                            ProposedPassword = new_password,
                            AccessToken = token
    )

    message("Password changed successfully.")
    invisible(NULL)

  }, error = function(e) {
    stop("Failed to change password: ", e$message)
  })
}
