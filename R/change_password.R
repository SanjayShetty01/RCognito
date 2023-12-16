change_password <- function(old_password, new_password, token) {

  tryCatch({
    cognito <-  paws::cognitoidentityprovider()

    cognito$change_password(PreviousPassword = old_password,
                            ProposedPassword = new_password,
                            AccessToken = token
    )

  }, error = function(e) {
    stop(e)
  })
}
