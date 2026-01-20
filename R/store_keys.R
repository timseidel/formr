#' Store Credentials in Keyring
#'
#' Securely stores formr credentials in the system keyring.
#' This function supports two modes:
#' 1. **Legacy Mode:** Stores email/password (and optional 2FA) for a specific account name.
#' 2. **API Mode:** Stores OAuth credentials or Access Tokens for a specific host.
#'
#' @param account_name (Legacy) A shorthand name for the account. If provided, Legacy mode is triggered.
#' @param email (Legacy) Email address for the account. Will be prompted if omitted.
#' @param secret_2fa (Legacy) A 2FA secret. Set to NULL to be prompted, or "" if not used.
#' @param host (API) The API URL (e.g., https://formr.org). Defaults to formr.org.
#' @param client_id (API) OAuth Client ID.
#' @param client_secret (API) OAuth Client Secret.
#' @param access_token (API) Direct Personal Access Token (alternative to OAuth).
#'
#' @export
#' @examples
#' \dontrun{
#' # --- LEGACY EXAMPLES ---
#' # Prompts for password interactively
#' formr_store_keys("formr_diary_study_account")
#'
#' # --- NEW API EXAMPLES ---
#' # Store Personal Access Token
#' formr_store_keys(access_token = "your-token-here")
#'
#' # Store OAuth Credentials for a custom host
#' formr_store_keys(host = "http://localhost",
#'                  client_id = "my-id",
#'                  client_secret = "my-secret")
#' }
formr_store_keys <- function(account_name = NULL,
														 email = NULL,
														 secret_2fa = NULL,
														 host = "https://formr.org",
														 client_id = NULL,
														 client_secret = NULL,
														 access_token = NULL) {
	
	if (!requireNamespace("keyring", quietly = TRUE)) {
		stop("Package 'keyring' is required.")
	}
	
	# --- LOGIC BRANCH 1: LEGACY MODE ---
	# Triggered if the user provides a positional argument or explicitly sets account_name
	if (!is.null(account_name)) {
		
		if (is.null(email)) {
			email <- readline("Enter your email: ")
		}
		
		# Store main password (interactive prompt via keyring)
		keyring::key_set(service = account_name, username = email)
		
		# Store 2FA Secret
		if (!is.null(secret_2fa)) {
			keyring::key_set_with_value(
				service = account_name, 
				username = paste(email, "2FA"),
				password = secret_2fa
			)
		} else {
			# Interactive prompt for 2FA
			keyring::key_set(
				service = account_name,
				username = paste(email, "2FA"),
				prompt = "2FA secret if applicable"
			)
		}
		
		message("[SUCCESS] Legacy credentials stored for account: ", account_name)
		return(invisible(NULL))
	}
	
	# --- LOGIC BRANCH 2: NEW API MODE ---
	# Triggered if account_name is NULL.
	
	# Validation: Ensure the user actually provided new credentials
	if (is.null(access_token) && (is.null(client_id) || is.null(client_secret))) {
		stop("Invalid usage. Please provide either a legacy 'account_name' OR API credentials (access_token OR client_id + client_secret).")
	}
	
	service_name <- paste0("formr_", host)
	
	if (!is.null(access_token)) {
		keyring::key_set_with_value(
			service = service_name,
			username = "access_token",
			password = access_token
		)
		message("[SUCCESS] Access Token stored for ", host)
		
	} else if (!is.null(client_id) && !is.null(client_secret)) {
		keyring::key_set_with_value(
			service = service_name,
			username = "client_id",
			password = client_id
		)
		keyring::key_set_with_value(
			service = service_name,
			username = "client_secret",
			password = client_secret
		)
		message("[SUCCESS] OAuth Credentials stored for ", host)
	}
}