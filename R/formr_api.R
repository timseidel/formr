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
  invisible(result)
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
	
	# 1. Start building the flat request list
	request_list <- list()
	
	# 2. Add the run name (required)
	request_list["run[name]"] <- run_name
	
	# 3. Add sessions (if provided)
	if (!is.null(sessions)) {
		# The PHP code `explode(',', ...)`
		# so we must collapse the vector into a single string.
		request_list["run[sessions]"] <- paste(sessions, collapse = ",")
	}
	
	# 4. Add surveys (if provided)
	if (!is.null(surveys) && is.list(surveys)) {
		
		# Loop through the named list
		for (survey_name in names(surveys)) {
			items <- surveys[[survey_name]]
			
			# This is the name the API expects, e.g., "surveys[all_widgets]"
			api_survey_name <- paste0("surveys[", survey_name, "]")
			
			if (is.null(items)) {
				# If NULL, we don't add an item list, which the PHP
				# will interpret as "get all items"
				request_list[api_survey_name] <- NA # or NULL, httr handles it
			} else {
				# Collapse the item vector into "item1,item2,item3"
				request_list[api_survey_name] <- paste(items, collapse = ",")
			}
		}
	}
	
	# 5. Call the current function
	raw_results <- formr_api_results(request = request_list)
	
	# 6 binding rows
	tidy_results <- lapply(raw_results, bind_rows)
	
	# 8. --- simplifying ---
	if (length(tidy_results) == 1) {
		return(tidy_results[[1]])
	} else {
		return(tidy_results)
	}
}
