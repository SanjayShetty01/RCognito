forgot_password <- function(client_id, username) {
  tryCatch({
      cognito <-  paws::cognitoidentityprovider()
      cognito$forgot_password(ClientId = client_id, Username = username)
    return("Forgot Password Request sent Successfully!")
  }, error = function(e) {
    stop(e)
  })
}
