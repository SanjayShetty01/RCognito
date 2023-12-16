library(paws)

sign_in <- function(client_id, username, password, authflow) {
  tryCatch({
    cognito <-  paws::cognitoidentityprovider()

    auth_parameter <-  list(USERNAME = username,
                            PASSWORD = password)

    user_info <- cognito$initiate_auth(AuthFlow = "USER_PASSWORD_AUTH",
                                       AuthParameters = auth_parameter,
                                       ClientId = client_id)

    return(user_info)

  }, error = function(e) {
    stop(e)
  }
  )
}
