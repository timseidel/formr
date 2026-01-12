# Getting Started

**To install the API-DEV version:**

``` r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("timseidel/formr")
```

## Authentication

To use the API features (fetching results, managing files), you must
first authenticate. You can store your credentials securely in your
system’s keyring so you don’t have to type them every time.

1.  Log in to formr.org and go to **Account \> API Settings**.
2.  Create a Client ID and Secret.

``` r
library(formr)

# Store your credentials once
# This saves them securely in your OS credential store
formr_store_keys(
  host = "https://formr.org", # or the URL of your instance
  client_id = "YOUR_CLIENT_ID", 
  client_secret = "YOUR_CLIENT_SECRET"
)

# Then, in all of your scripts, simply run:
formr_api_authenticate(host = "https://formr.org") 
# or the URL of your instance
```

#### You can use the API within your runs R-Code.

``` r
# Simply run:
formr_api_authenticate()
```

#### Running API Functions

Once authenticated, you can run the API functions:

``` r
# For example:
formr_runs()
```

#### Logging off

[`formr_api_authenticate()`](http://rubenarslan.github.io/formr/reference/formr_api_authenticate.md)
provides you with a TOKEN that is valid for an Hour. Currently, you will
need to rerun the authentication after this.

**At the end of your analysis:**

- **On the Server:** When running Code on the formr server, the TOKEN
  gets invalidated automatically.
- **Local Device:** When running Code on your local device, you may let
  the TOKEN run out its validity period or revoke it manually.

``` r
formr_api_logout()
```
