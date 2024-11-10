#' Confirm User Sign Up
#'
#' This function confirms the sign-up process for a user in AWS Cognito.
#'
#' @import paws
#'
#' @param client_id A character string representing the app client ID.
#' @param userpool A character string representing the user pool ID.
#' @param username A character string for the user's username.
#' @param verification_code A character string for the verification code sent to the user.
#'
#' @return Nothing (displays a message on success or raises an error if it fails).
#' @export

confirm_sign_up_user <- function(client_id,
                            userpool,
                            username,
                            verification_code) {
  tryCatch({
    cognito <- paws::cognitoidentityprovider()

    users_info_from_cognito <- get_user_list_from_cognito(cognito_client =
                                                            cognito, userpool =
                                                            userpool)

    base::stopifnot(
      "User not found, Please enter correct username" =
        username %in% users_info_from_cognito$usernames
    )

    cognito$confirm_sign_up(
      ClientId = client_id,
      Username = username,
      ConfirmationCode = verification_code
    )

    # Display a success message
    message("User sign-up confirmed successfully.")
    invisible(NULL)

  }, error = function(e) {
    # Provide a custom error message for clarity
    stop("Failed to confirm sign-up: ", e$message)
  })
}
