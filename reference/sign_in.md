# Sign in to AWS Cognito

Sign in to AWS Cognito

## Usage

``` r
sign_in(client_id, username, password, authflow = "USER_PASSWORD_AUTH")
```

## Arguments

- client_id:

  A character string representing the app client ID.

- username:

  A character string for the user's username.

- password:

  A character string for the user's password.

- authflow:

  Authentication flow type. Only "USER_PASSWORD_AUTH" is currently
  supported.

## Value

A list with user information and tokens if successful; throws an error
otherwise.
