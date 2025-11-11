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

formr_api_authenticate <- function(client_id = NULL, client_secret = NULL,
																	 host = "https://api.formr.org", ott = NULL) {
	
	ott_val <- if (!is.null(ott)) ott else Sys.getenv("FORMR_API_OTT", unset = "")
	host_val <- if (!is.null(host)) host else Sys.getenv("FORMR_API_HOST", unset = "https://api.formr.org")
	
	if (nzchar(ott_val)) {
		message("Authenticating using secure one-time-token.")
		base_url <- httr::parse_url(host_val)
		base_url$query <- list(access_token = ott_val)
		.formr_current_session$set(base_url)
		return(invisible(TRUE))
	}
	
	if (!is.null(client_id) && !is.null(client_secret)) {
		message("Authenticating using client credentials.")
		formr_api_access_token(client_id, client_secret, host_val)
		return(invisible(TRUE))
	}
	
	stop("Could not find API credentials. \n",
			 " - If running locally, please provide client_id and client_secret.\n",
			 " - If running in the webapp, supply OTT or set FORMR_API_OTT.",
			 call. = FALSE)
}
