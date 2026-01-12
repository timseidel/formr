# Authenticate with formr

Connects to the API. If no credentials are provided, it tries to find
them in the keyring.

## Usage

``` r
formr_api_authenticate(
  host = "https://formr.org",
  client_id = NULL,
  client_secret = NULL,
  access_token = NULL
)
```

## Arguments

- host:

  API Base URL.

- client_id:

  OAuth Client ID.

- client_secret:

  OAuth Client Secret.

- access_token:

  Direct Access Token.
