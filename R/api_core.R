#' Internal environment to store session state
#' @noRd
.formr_state <- new.env(parent = emptyenv())

#' Get Current API session
#' @export
formr_api_session <- function() {
	if (exists("session", envir = .formr_state)) {
		return(get("session", envir = .formr_state))
	}
	return(NULL)
}

#' Store API Credentials in Keyring
#'
#' Securely stores your formr credentials in the system keyring.
#' You can store either OAuth credentials (client_id/secret) OR a direct access token.
#'
#' @param host The API URL (e.g. https://formr.org or http://localhost)
#' @param client_id OAuth Client ID
#' @param client_secret OAuth Client Secret
#' @param access_token Direct Personal Access Token (alternative to OAuth)
#' @export
formr_store_keys <- function(host = "https://formr.org",
														 client_id = NULL,
														 client_secret = NULL,
														 access_token = NULL) {
	if (!requireNamespace("keyring", quietly = TRUE))
		stop("Package 'keyring' is required.")
	
	service_name <- paste0("formr_", host)
	
	if (!is.null(access_token)) {
		keyring::key_set_with_value(service = service_name,
																username = "access_token",
																password = access_token)
		message("✅ Access Token stored for ", host)
	} else if (!is.null(client_id) && !is.null(client_secret)) {
		keyring::key_set_with_value(service = service_name,
																username = "client_id",
																password = client_id)
		keyring::key_set_with_value(service = service_name,
																username = "client_secret",
																password = client_secret)
		message("✅ OAuth Credentials stored for ", host)
	} else {
		stop("Please provide either (client_id + client_secret) OR access_token.")
	}
}

#' Authenticate with formr
#'
#' Connects to the API. If no credentials are provided, it tries to find them in the keyring.
#'
#' @param host API Base URL.
#' @param client_id OAuth Client ID.
#' @param client_secret OAuth Client Secret.
#' @param access_token Direct Access Token.
#' @export
formr_api_authenticate <- function(host = "https://formr.org",
																	 client_id = NULL,
																	 client_secret = NULL,
																	 access_token = NULL) {
	
	# 1. Try to load from Keyring if missing
	if (is.null(client_id) && is.null(access_token) && requireNamespace("keyring", quietly = TRUE)) {
		service_name <- paste0("formr_", host)
		try({
			keys <- keyring::key_list(service = service_name)
			if ("access_token" %in% keys$username) {
				access_token <- keyring::key_get(service = service_name, username = "access_token")
			} else if (all(c("client_id", "client_secret") %in% keys$username)) {
				client_id <- keyring::key_get(service = service_name, username = "client_id")
				client_secret <- keyring::key_get(service = service_name, username = "client_secret")
			}
		}, silent = TRUE)
	}
	
	# 2. Authenticate
	if (!is.null(access_token)) {
		# Direct Token
		session_data <- list(base_url = httr::parse_url(host), token = access_token)
		
		# --- CHANGE: Save to environment ---
		assign("session", session_data, envir = .formr_state)
		
		# Verify
		tryCatch({
			formr_api_request("user/me", method = "GET")
			message("✅ Authenticated via Access Token.")
		}, error = function(e) {
			warning("Authentication failed: ", e$message)
		})
		
	} else if (!is.null(client_id) && !is.null(client_secret)) {
		# Client Credentials Flow
		token_url <- httr::parse_url(host)
		token_url$path <- paste0(token_url$path, "/oauth/access_token")
		token_url$path <- gsub("//", "/", token_url$path)
		
		res <- httr::POST(
			token_url,
			httr::authenticate(client_id, client_secret, type = "basic"),
			body = list(grant_type = "client_credentials"),
			encode = "form"
		)
		
		if (httr::status_code(res) >= 400)
			stop("OAuth Error: ", httr::content(res, "text"))
		
		token <- httr::content(res)$access_token
		
		# --- CHANGE: Save to environment ---
		session_data <- list(base_url = httr::parse_url(host), token = token)
		assign("session", session_data, envir = .formr_state)
		
		message("✅ Authenticated via OAuth.")
		
	} else {
		stop("No credentials found. Use formr_store_keys() or provide arguments.")
	}
}

#' Internal: API Request Handler (Robust Version)
#' @noRd
formr_api_request <- function(endpoint,
															method = "GET",
															body = NULL,
															query = NULL,
															api_version = "v1",
															encode = NULL) {
	session <- formr_api_session()
	if (is.null(session))
		stop("Not authenticated. Run formr_api_authenticate().")
	
	url <- session$base_url
	url$path <- paste0(url$path, "/", api_version, "/", endpoint)
	url$path <- gsub("//", "/", url$path)
	
	auth <- httr::add_headers(Authorization = paste("Bearer", session$token))
	
	if (is.null(encode)) {
		encode <- "json"
		if (any(sapply(body, inherits, "form_file")))
			encode <- "multipart"
	}
	
	if (method == "GET")
		req <- httr::GET(url, query = query, auth)
	else if (method == "POST")
		req <- httr::POST(url, body = body, encode = encode, auth)
	else if (method == "PUT")
		req <- httr::PUT(url, body = body, encode = encode, auth)
	else if (method == "PATCH")
		req <- httr::PATCH(url, body = body, encode = encode, auth)
	else if (method == "DELETE")
		req <- httr::DELETE(url, auth)
	
	if (httr::http_type(req) != "application/json") {
		stop(
			sprintf(
				"API Error: Expected JSON, got %s.\nPreview: %s",
				httr::http_type(req),
				substr(httr::content(req, "text"), 1, 200)
			)
		)
	}
	
	if (httr::status_code(req) >= 400) {
		stop(sprintf(
			"API Error (%s): %s",
			httr::status_code(req),
			httr::content(req, "text")
		))
	}
	
	httr::content(req, "parsed")
}