# Store API Credentials in Keyring

Securely stores your formr credentials in the system keyring. You can
store either OAuth credentials (client_id/secret) OR a direct access
token.

## Usage

``` r
formr_store_keys(
  host = "https://formr.org",
  client_id = NULL,
  client_secret = NULL,
  access_token = NULL
)
```

## Arguments

- host:

  The API URL (e.g. https://formr.org or http://localhost)

- client_id:

  OAuth Client ID

- client_secret:

  OAuth Client Secret

- access_token:

  Direct Personal Access Token (alternative to OAuth)
