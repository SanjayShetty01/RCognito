#' Check if an email address is valid.
#'
#' This function validates the format of an email address.
#'
#' @param email The email address to be validated.
#' @return TRUE if the email is valid, otherwise an error is thrown.
is_valid_email <- function(email) {
  email_pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}$"

  stopifnot("Invalid Email Address" = grepl(email_pattern, email))

  TRUE
}

#' Check if an access token is valid.
#'
#' This function validates the format of an access token.
#'
#' @param token The access token to be validated.
#' @return TRUE if the token is valid, otherwise an error is thrown.
is_valid_token <- function(token) {
  # I don't trust this completely, might fail
  string_pattern <- "^eyJ[[:alnum:]=_-]+\\.[[:alnum:]=_-]+\\.[[:alnum:]=_-]+$"
  stopifnot("Invalid Access Token" = grepl(string_pattern, token))
  TRUE
}

#' Get User List from Cognito
#'
#' Retrieve the list of usernames and email addresses from Cognito.
#'
#' @param cognito_client The Cognito client.
#' @param userpool A character string representing the user pool ID.
#' @return A list containing usernames and email addresses.
get_user_list_from_cognito <- function(cognito_client, userpool) {

  user_info_from_cognito <- cognito_client$list_users(UserPoolId =
                                                        userpool,
                                                      AttributesToGet =
                                                        list("email"))

  usernames <- base::sapply(user_info_from_cognito$Users, \(x) x$Username)
  email_id <- base::sapply(user_info_from_cognito$Users,
                           \(x) x$Attributes[[1]]$Value)

  return(list(usernames = usernames, email_id = email_id))
}
