library(paws)

sign_up <- function(client_id, email, username, password, ...){

  tryCatch({
    cognito <-  paws::cognitoidentityprovider()
    cognito$sign_up(ClientId = client_id, Username = username,
                    Password = password,
                    UserAttributes = list(
                      list(Name = "email",
                           Value = email),
                      ...
                    ))
  }, error = function(e) {
    stop(e)
  })
}
