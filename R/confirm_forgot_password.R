confirm_forgot_password <- function(client_id, username,
                                    confirmation_code, new_password) {
  tryCatch({
    cognito <-  paws::cognitoidentityprovider()
    cognito$confirm_forgot_password(ClientId = client_id, Username = username,
                                    ConfirmationCode = confirmation_code,
                                    Password = new_password)

  return("New Password set successfully")
  }, error = function(e) {
    stop(e)
  })
}
