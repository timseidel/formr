# Store Credentials in Keyring

Securely stores formr credentials in the system keyring. This function
supports two modes:

1.  **Legacy Mode:** Stores email/password (and optional 2FA) for a
    specific account name.

2.  **API Mode:** Stores OAuth credentials or Access Tokens for a
    specific host.

## Usage

``` r
formr_store_keys(
  account_name = NULL,
  email = NULL,
  password = NULL,
  secret_2fa = NULL,
  host = "https://formr.org",
  client_id = NULL,
  client_secret = NULL,
  access_token = NULL
)
```

## Arguments

- account_name:

  (Legacy) A shorthand name for the account. If provided, Legacy mode is
  triggered.

- email:

  (Legacy) Email address for the account. Will be prompted if omitted.

- password:

  (Legacy) Optional. Provide to skip interactive prompt (useful for
  scripts/tests).

- secret_2fa:

  (Legacy) A 2FA secret. Set to NULL to be prompted, or "" if not used.

- host:

  (API) The API URL (e.g., https://formr.org). Defaults to formr.org.

- client_id:

  (API) OAuth Client ID.

- client_secret:

  (API) OAuth Client Secret.

- access_token:

  (API) Direct Personal Access Token (alternative to OAuth).

## Examples

``` r
if (FALSE) { # \dontrun{
# --- LEGACY EXAMPLES ---
# Prompts for password interactively
formr_store_keys("formr_diary_study_account")

# --- NEW API EXAMPLES ---
# Store Personal Access Token
formr_store_keys(access_token = "your-token-here")

# Store OAuth Credentials for a custom host
formr_store_keys(host = "http://localhost",
                 client_id = "my-id",
                 client_secret = "my-secret")
} # }
```
