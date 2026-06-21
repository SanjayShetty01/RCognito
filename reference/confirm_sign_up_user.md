# Confirm User Sign Up

This function confirms the sign-up process for a user in AWS Cognito.

## Usage

``` r
confirm_sign_up_user(client_id, userpool, username, verification_code)
```

## Arguments

- client_id:

  A character string representing the app client ID.

- userpool:

  A character string representing the user pool ID.

- username:

  A character string for the user's username.

- verification_code:

  A character string for the verification code sent to the user.

## Value

Nothing (displays a message on success or raises an error if it fails).
