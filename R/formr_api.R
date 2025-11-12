#' Connect to formr API
#'
#' Connects to formr using your client_id and client_secret (OAuth 2.0 grant type: client_credentials).
#'
#' @param client_id your client_id
#' @param client_secret your client_secret
#' @param host defaults to https://formr.org
#' @export
#' @examples
#' @importFrom dplyr bind_rows
#' \dontrun{
#' formr_api_access_token(client_id = 'your_id', client_secret = 'your_secret' )
#' }

formr_api_access_token = function(client_id, client_secret, host = "https://api.formr.org/") {
	base_url = httr::parse_url(host)
	
	.formr_current_session$set(base_url)
	token_url = base_url
	token_url$path = paste0(token_url$path, "oauth/access_token")
	
	result = httr::POST(url = token_url, body = list(client_id = client_id, 
																									 client_secret = client_secret, grant_type = "client_credentials"))
	
	token = httr::content(result)
	
	if (result$status_code != 200 & is.null(token$error)) {
		stop("Connection error using formr API: ", token)
	} else if (!is.null(token$error)) {
		stop("Error using formr API: ", token$error_code, " ", 
				 token$error, " ", token$description)
	}
	base_url$query = list(access_token = token$access_token)
	.formr_current_session$set(base_url)
	message("Successfully connected to formr API")
	print(result)
}

.store_formr_current_session <- function() {
	.formr_store_current_session <- NULL
	
	list(get = function() .formr_store_current_session, set = function(value) .formr_store_current_session <<- value)
}
.formr_current_session <- .store_formr_current_session()


#' Get current API session
#' Return or set URL in list form for formr API (if available)
#' @export
#'
formr_api_session = function() {
	.formr_current_session$get()
}

#' Get result from formr
#'
#' After obtaining a token from formr, use this request
#'
#' @param request parameter (see example, API docs)
#' @param token defaults to last used token
#' 
#' @export
#' @examples
#' \dontrun{
#' request <- 
#' 	list(
#' 		"run[name]" = 'widgets',
#' 		"run[sessions]" = 
#' 		  'PJ_nACjFQDEBhx7pMUfZQz3mV-OtetnpEdqT88aiY8eXE4-HegFI7Sri4yifxPXO',
#' 		"surveys[all_widgets]" = "abode, yourstory, mc_god"
#' )
#' formr_api_results(request)
#' }

formr_api_results = function(request = NULL, token = NULL) {
	stopifnot(!is.null(request))
	get_url = formr_api_session()
	if (!is.null(token)) {
		get_url = token
	}
	get_url$path = paste0(get_url$path, "get/results")
	result = httr::GET(get_url, query = request)
	res = httr::content(result)
	res
}

# Testing Additions by Tim

#' Get results from the formr API (User-Friendly Wrapper)
#'
#' After authenticating with [formr_api_access_token()], this function
#' fetches results with easy-to-use R arguments.
#'
#' @param run_name The name of the run (required).
#' @param surveys A named list, where names are survey names and values are
#'   character vectors of items. e.g., `list(survey_a = c("item1", "item2"))`.
#'   Use `NULL` as the value to get all items for that survey.
#' @param sessions A character vector of session IDs to filter for.
#'
#' @export
#' @examples
#' \dontrun{
#' # First, authenticate
#' formr_api_access_token(client_id = 'your_id', client_secret = 'your_secret')
#' 
#' # --- Example 1: Get all data from the 'widgets' run ---
#' all_data <- formr_get_results(run_name = "widgets")
#' 
#' # --- Example 2: Get specific items from two surveys ---
#' my_surveys <- list(
#'   all_widgets = c("abode", "mc_god"),
#'   demographics = c("age", "gender")
#' )
#' survey_data <- formr_get_results(run_name = "widgets", surveys = my_surveys)
#' 
#' # --- Example 3: Get data for specific users ---
#' my_sessions <- c("session_id_abc", "session_id_xyz")
#' user_data <- formr_get_results(run_name = "widgets", sessions = my_sessions)
#' 
#' }
formr_get_results <- function(run_name, surveys = NULL, sessions = NULL) {
	# If multiple runs are provided, call recursively and return a named list
	if (length(run_name) > 1) {
		runs <- as.character(run_name)
		out <- lapply(runs, function(rn) {
			formr_get_results(run_name = rn, surveys = surveys, sessions = sessions)
		})
		names(out) <- runs
		return(out)
	}
	
	# Validate single run_name
	if (!is.character(run_name) || length(run_name) != 1 || !nzchar(run_name)) {
		stop("run_name must be a non-empty character scalar or a character vector of run names.")
	}
	
	request_list <- list()
	request_list[["run[name]"]] <- run_name
	
	# Handle sessions: NULL or "" -> omit; otherwise collapse to CSV
	if (!is.null(sessions)) {
		if (!is.character(sessions)) sessions <- as.character(sessions)
		sessions <- trimws(sessions)
		sessions <- sessions[nzchar(sessions)]
		if (length(sessions) > 0) {
			request_list[["run[sessions]"]] <- paste(unique(sessions), collapse = ",")
		}
	}
	
	# Handle surveys
	if (!is.null(surveys)) {
		if (is.character(surveys) && is.null(names(surveys))) {
			# Character vector of survey names -> all items for each
			for (survey_name in surveys) {
				if (!nzchar(survey_name)) next
				request_list[[paste0("surveys[", survey_name, "]")]] <- ""
			}
		} else if (is.list(surveys)) {
			# Named list: survey -> items (NULL, "" or length-0 -> all items)
			if (is.null(names(surveys)) || any(!nzchar(names(surveys)))) {
				stop("When 'surveys' is a list, it must be a named list: list(survey_name = items).")
			}
			for (survey_name in names(surveys)) {
				items <- surveys[[survey_name]]
				key   <- paste0("surveys[", survey_name, "]")
				
				if (is.null(items) ||
						(is.character(items) && length(items) == 1 && items == "") ||
						(is.character(items) && length(items) == 0)) {
					# All items for this survey
					request_list[[key]] <- ""
				} else {
					# Specific items (character vector or CSV string)
					if (!is.character(items)) items <- as.character(items)
					items <- trimws(items)
					items <- items[nzchar(items)]
					request_list[[key]] <- paste(items, collapse = ",")
				}
			}
		} else {
			stop("Argument 'surveys' must be NULL, a character vector of survey names, or a named list mapping survey -> items.")
		}
	}
	
	# Call the lower-level function
	raw_results <- formr_api_results(request = request_list)
	
	# Bind/return results safely
	tidy_results <- lapply(raw_results, function(x) {
		if (is.null(x)) return(NULL)
		if (is.data.frame(x)) return(x)
		tryCatch(
			dplyr::bind_rows(x),
			error = function(e) x
		)
	})
	tidy_results <- tidy_results[!vapply(tidy_results, is.null, logical(1))]
	
	if (length(tidy_results) == 1) tidy_results[[1]] else tidy_results
}

#' Authenticate with the formr API
#'
#' Establishes an authenticated session with the formr API using either an access token
#' or OAuth client credentials. This is a convenience wrapper around 
#' [formr_api_access_token()] that handles multiple authentication scenarios.
#'
#' @param client_id Character string. The OAuth Client ID for authentication.
#'   Required when using client credentials authentication (typically for local development).
#' @param client_secret Character string. The OAuth Client Secret for authentication.
#'   Required when using client credentials authentication (typically for local development).
#' @param host Character string. The base URL of the formr API instance.
#'   Defaults to "https://api.formr.org".
#' @param access_token Character string. A pre-generated access token for authentication.
#'   When provided, this takes precedence over client credentials. If NULL (default),
#'   the function will check for an \code{access_token} variable in the calling environment.
#'   This is the preferred method when running code within the formr web application 
#'   (OpenCPU environment).
#'
#' @details
#' The function supports two authentication methods with the following priority:
#'
#' \enumerate{
#'   \item **Access Token (Direct):** If \code{access_token} is provided and non-empty,
#'   or if an \code{access_token} variable exists in the calling environment, it will 
#'   be used directly for API authentication without making an OAuth request. This method 
#'   is recommended when running within the formr OpenCPU environment.
#'   \item **Client Credentials (OAuth 2.0):** If both \code{client_id} and 
#'   \code{client_secret} are provided, the function calls [formr_api_access_token()]
#'   to obtain an access token via the OAuth 2.0 client credentials grant flow.
#'   This method is suitable for local development and external scripts.
#' }
#'
#' At least one authentication method must be provided. If neither method has the required
#' parameters, the function will raise an error with guidance on proper usage.
#'
#' After successful authentication, the session information is stored internally and will
#' be used automatically by other formr API functions like [formr_get_results()].
#' You can retrieve the current session with [formr_api_session()].
#'
#' @return Returns \code{TRUE} invisibly upon successful authentication.
#' 
#' @seealso 
#' \code{\link{formr_api_access_token}} for the underlying OAuth authentication,
#' \code{\link{formr_api_session}} to retrieve the current session,
#' \code{\link{formr_get_results}} to fetch data after authentication.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Method 1: Using Client Credentials (for local development)
#' formr_api_authenticate(
#'   client_id = "your_client_id",
#'   client_secret = "your_client_secret"
#' )
#'
#' # Method 2: Using Access Token directly
#' formr_api_authenticate(access_token = "your_token_here")
#'
#' # Method 3: Using Access Token from environment variable
#' # Set environment variable first: Sys.setenv(FORMR_ACCESS_TOKEN = "your_token")
#' formr_api_authenticate()  # Will automatically use FORMR_ACCESS_TOKEN
#'
#' # Method 4: Custom host with client credentials
#' formr_api_authenticate(
#'   client_id = "your_client_id",
#'   client_secret = "your_client_secret",
#'   host = "https://custom-formr-instance.org"
#' )
#' 
#' # After authentication, you can fetch data
#' results <- formr_get_results(run_name = "my_study")
#' }
formr_api_authenticate <- function(client_id = NULL, client_secret = NULL,
																	 host = "https://api.formr.org", access_token = NULL) {
	
	# Check for access_token in R environment if not provided
	if (is.null(access_token) && exists("access_token", envir = parent.frame())) {
		access_token <- get("access_token", envir = parent.frame())
	}
	
	# Use access token if available
	if (nzchar(access_token)) {
		message("Authenticating using access_token.")
		base_url <- httr::parse_url(host)
		base_url$query <- list(access_token = access_token)
		.formr_current_session$set(base_url)
		return(invisible(TRUE))
	}
	
	# Fall back to client credentials
	if (!is.null(client_id) && !is.null(client_secret)) {
		message("Authenticating using client credentials.")
		formr_api_access_token(client_id, client_secret, host)
		return(invisible(TRUE))
	}
	
	# No valid authentication method found
	stop("Could not find API credentials. \n",
			 " - If running locally, please provide client_id and client_secret.\n",
			 " - If running within formr (opencpu), ensure an access_token variable exists in your workspace,\n",
			 "   or supply the access_token parameter directly.",
			 call. = FALSE)
}
