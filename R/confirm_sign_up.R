library(paws)

confirm_sign_up <- function(client_id, username, verification_code) {
  tryCatch({
    cognito <-  paws::cognitoidentityprovider()
    cognito$confirm_sign_up(ClientId = client_id, Username = username,
                            ConfirmationCode = verification_code)
  }, error = function(e) {
    stop(e)
  })
}
