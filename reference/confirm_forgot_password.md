# Confirm Forgot Password

This function confirms the forgot password process by setting a new
password for a user.

## Usage

``` r
confirm_forgot_password(client_id, username, confirmation_code, new_password)
```

## Arguments

- client_id:

  A character string representing the app client ID.

- username:

  A character string for the user's username.

- confirmation_code:

  A character string for the confirmation code sent to the user.

- new_password:

  A character string for the user's new password.

## Value

Nothing (displays a message on success or raises an error if it fails).
