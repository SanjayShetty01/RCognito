# Sign up a new user to AWS Cognito

Sign up a new user to AWS Cognito

## Usage

``` r
sign_up_user(client_id, email, username, password, ...)
```

## Arguments

- client_id:

  A character string representing the app client ID.

- email:

  A character string for the user's email address.

- username:

  A character string for the user's username.

- password:

  A character string for the user's password.

- ...:

  Additional named user attributes for Cognito (e.g., \`phone_number\`).

## Value

A list with sign-up confirmation details if successful; throws an error
otherwise.
