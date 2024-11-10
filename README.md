# RCognito

RCognito is an R package designed to simplify interaction with Amazon Cognito services through a user-friendly API wrapper. Seamlessly authenticate users, manage user pools, and perform common Cognito operations in R. Empower your R applications with secure and scalable identity management using Rcognito.

## Installation

You can install the `RCognito` package directly from GitHub using the `remotes` package:

```r
# Install remotes package if not already installed
install.packages("remotes")

# Install RCognito from GitHub
remotes::install_github("SanjayShetty01/RCognito")
```

## Setup AWS Credentials

Before using the RCognito package, make sure to set your AWS credentials. You can do this in either of the following ways:

### Method 1: Set Credentials in `.Renviron` File

Create or edit the `.Renviron` file in your R home directory and add the following:

```
AWS_ACCESS_KEY_ID="your-access-key-id"
AWS_SECRET_ACCESS_KEY="your-secret-access-key"
AWS_REGION="your-region"
```

### Method 2: Use `Sys.setenv()` Function

Alternatively, you can set the AWS credentials programmatically using the `Sys.setenv()` function in your R script or session:

```r
# Set AWS credentials
Sys.setenv(AWS_ACCESS_KEY_ID = "your-access-key-id")
Sys.setenv(AWS_SECRET_ACCESS_KEY = "your-secret-access-key")
Sys.setenv(AWS_REGION = "your-region")
```

## Usage

### 1. Sign Up a New User

The `sign_up_user` function allows you to sign up a new user in AWS Cognito. It accepts the `client_id`, `email`, `username`, and `password` parameters.

```r
library(Rcognito)

# Sign up a new user
sign_up_user(
  client_id = "your-client-id",
  email = "user@example.com",
  username = "johndoe",
  password = "password123"
)
```

#### Arguments:
- `client_id`: Your AWS Cognito App client ID.
- `email`: The user's email address.
- `username`: The user's desired username.
- `password`: The user's password.

---

### 2. Confirm User Sign-Up

Once a user has signed up, you need to confirm their sign-up using the verification code sent to their email. The `confirm_sign_up_user` function helps with this.

```r
# Confirm user sign-up
confirm_sign_up_user(
  client_id = "your-client-id",
  userpool = "your-userpool-id",
  username = "johndoe",
  verification_code = "123456"
)
```

#### Arguments:
- `client_id`: Your AWS Cognito App client ID.
- `userpool`: Your AWS Cognito User Pool ID.
- `username`: The username of the user to confirm.
- `verification_code`: The verification code sent to the user's email.

---

### 3. Sign In a User

Once a user is confirmed, they can sign in using their `username` and `password` with the `sign_in` function.

```r
# Sign in a user
user_info <- sign_in(
  client_id = "your-client-id",
  username = "johndoe",
  password = "password123"
)

# View user information
print(user_info)
```

#### Arguments:
- `client_id`: Your AWS Cognito App client ID.
- `username`: The username of the user to sign in.
- `password`: The user's password.

---

### 4. Change User Password

Use the `change_password` function to change a user's password after they are authenticated.

```r
# Change user password
change_password(
  old_password = "old-password", 
  new_password = "new-password", 
  token = "user-access-token"
)
```

#### Arguments:
- `old_password`: The current password of the user.
- `new_password`: The new password for the user.
- `token`: The user's access token.

---

### 5. Forgot Password Request

If a user forgets their password, use the `forgot_password` function to initiate the password reset process.

```r
# Initiate forgot password request
forgot_password(
  client_id = "your-client-id", 
  username = "johndoe"
)
```

#### Arguments:
- `client_id`: Your AWS Cognito App client ID.
- `username`: The username of the user who forgot their password.

---

### 6. Confirm Forgot Password

After receiving a confirmation code, use the `confirm_forgot_password` function to set a new password.

```r
# Confirm forgot password with new password and confirmation code
confirm_forgot_password(
  client_id = "your-client-id", 
  username = "johndoe", 
  confirmation_code = "confirmation-code", 
  new_password = "new-password"
)
```

#### Arguments:
- `client_id`: Your AWS Cognito App client ID.
- `username`: The username of the user confirming their forgot password.
- `confirmation_code`: The confirmation code sent to the user.
- `new_password`: The new password the user wants to set.


## Use Cases for `RCognito`


### 1. **Authentication for Shiny Applications**
`RCognito` allows you to integrate AWS Cognito for user authentication in Shiny applications. It enables secure user sign-up, sign-in, password management and session management, making restricting access to sensitive features based on authenticated user roles easier.

---

### 2. **Securing Plumber API Endpoints**
`RCognito` can be used to secure API endpoints in a Plumber service by validating AWS Cognito authentication tokens. This ensures that only authorized users can access specific API endpoints and perform actions that require authentication.

---

### 3. **Integrating Cognito Authentication with R Scripts in Backend**
When using R scripts for backend operations (e.g., data analysis, API interaction), `RCognito` facilitates secure authentication by verifying user credentials before allowing access to sensitive tasks or data.

---

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
