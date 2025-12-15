#' List Sessions in a Run
#' 
#' Returns a paginated, tidy data frame of sessions.
#' 
#' @param run_name Name of the run.
#' @param active Filter: TRUE for ongoing, FALSE for finished, NULL for all.
#' @param testing Filter: TRUE for test sessions, FALSE for real users, NULL for all.
#' @param limit Pagination limit (default 100).
#' @param offset Pagination offset (default 0).
#' @return A data.frame containing session details (code, position, last_access, etc.).
#' @importFrom dplyr bind_rows mutate
#' @export
formr_sessions <- function(run_name, active = NULL, testing = NULL, limit = 100, offset = 0) {
	# Prepare query parameters
	query <- list(limit = limit, offset = offset)
	
	# Map active (TRUE/FALSE) to API strings ("true"/"false") or 1/0
	# Source: ApiHelperV1.php listSessions
	if (!is.null(active)) query$active <- if(active) 1 else 0
	if (!is.null(testing)) query$testing <- if(testing) 1 else 0
	
	res <- formr_api_request(paste0("runs/", run_name, "/sessions"), query = query)
	
	if (length(res) == 0) {
		message(sprintf("ℹ No sessions found for run '%s'.", run_name))
		return(data.frame())
	}
	
	# Convert to tidy data frame
	dplyr::bind_rows(res) %>%
		dplyr::mutate(
			created = as.POSIXct(.data$created),
			last_access = as.POSIXct(.data$last_access),
			ended = as.POSIXct(.data$ended),
			testing = as.logical(.data$testing)
		)
}

#' Create Session(s)
#' 
#' Creates one or more sessions. If `codes` is NULL, one random session is created.
#' If `codes` is provided, tries to create sessions with those specific codes.
#' 
#' @param run_name Name of the run.
#' @param codes Character vector of codes. If NULL, creates one random code.
#' @param testing Logical. Mark these sessions as testing?
#' @return Invisibly returns the API response (including created sessions and any errors).
#' @export
formr_create_session <- function(run_name, codes = NULL, testing = FALSE) {
	body <- list(testing = if(testing) 1 else 0)
	
	# API expects 'code' to be an array if multiple, or a single value
	# Source: ApiHelperV1.php createSession
	if (!is.null(codes)) {
		body$code <- codes
	}
	
	res <- formr_api_request(
		endpoint = paste0("runs/", run_name, "/sessions"), 
		method = "POST", 
		body = body
	)
	
	# Provide user feedback based on the complex response structure (201, 207, 400)
	# Source: ApiHelperV1.php createSession
	count_created <- if(!is.null(res$count_created)) res$count_created else 0
	count_failed <- if(!is.null(res$count_failed)) res$count_failed else 0
	
	if (count_created > 0) {
		message(sprintf("✅ Successfully created %d session(s).", count_created))
		# Print the first few created codes
		if(length(res$sessions) > 0) {
			shown <- head(res$sessions, 5)
			cat(paste0("   Codes: ", paste(shown, collapse = ", ")))
			if(length(res$sessions) > 5) cat(" ...")
			cat("\n")
		}
	}
	
	if (count_failed > 0) {
		warning(sprintf("⚠️ Failed to create %d session(s).", count_failed))
		# Print the errors
		if(!is.null(res$errors)) {
			err_df <- dplyr::bind_rows(res$errors)
			print(err_df)
		}
	}
	
	invisible(res)
}

#' Perform Action on Session(s)
#' 
#' Controls the flow of one or more sessions.
#' 
#' @param run_name Name of the run.
#' @param session_codes A single code or vector of session codes.
#' @param action One of: "end_external", "toggle_testing", "move_to_position".
#' @param position Required only if action is "move_to_position".
#' @return A logical vector indicating success for each session.
#' @export
formr_session_action <- function(run_name, session_codes, action, position = NULL) {
	
	# 1. Validation
	valid_actions <- c("end_external", "toggle_testing", "move_to_position")
	if (!action %in% valid_actions) {
		stop("Invalid action. Must be one of: ", paste(valid_actions, collapse = ", "))
	}
	
	if (action == "move_to_position" && is.null(position)) {
		stop("Argument 'position' is required for action 'move_to_position'.")
	}
	
	# 2. Define single-action helper
	perform_one <- function(code) {
		tryCatch({
			body <- list(action = action)
			if (!is.null(position)) body$position <- position
			
			# Source: ApiHelperV1.php performSessionAction
			formr_api_request(
				endpoint = paste0("runs/", run_name, "/sessions/", code, "/actions"), 
				method = "POST", 
				body = body
			)
			return(TRUE)
			
		}, error = function(e) {
			warning(sprintf("⚠️ Failed to perform action on '%s': %s", code, e$message))
			return(FALSE)
		})
	}
	
	# 3. Iterate over all codes
	# We use vapply to get a clean named logical vector back
	results <- vapply(session_codes, perform_one, FUN.VALUE = logical(1))
	
	# 4. Feedback
	success_count <- sum(results)
	total_count <- length(session_codes)
	
	if (success_count == total_count) {
		message(sprintf("✅ Action '%s' successfully performed on all %d session(s).", action, total_count))
	} else if (success_count > 0) {
		message(sprintf("ℹ️ Action '%s' performed on %d/%d session(s). (See warnings for failures)", action, success_count, total_count))
	} else {
		warning(sprintf("❌ Action '%s' failed for all %d session(s).", action, total_count))
	}
	
	invisible(results)
}

#' Get Session Details (Vectorized & Tidy)
#' 
#' Retrieves detailed state for one or more sessions.
#' Returns a tidy tibble with flattened unit information.
#' 
#' @param run_name Name of the run.
#' @param session_codes A single code or a vector of session IDs/codes.
#' @return A tibble containing session details.
#' @importFrom dplyr bind_rows mutate as_tibble
#' @export
formr_session_details <- function(run_name, session_codes) {
	
	# Internal helper to fetch a single session
	fetch_one <- function(code) {
		tryCatch({
			# 1. Call API
			# Source: ApiHelperV1.php handleSessions -> getSessionDetails
			res <- formr_api_request(
				endpoint = paste0("runs/", run_name, "/sessions/", code),
				method = "GET"
			)
			
			# 2. Extract Nested "current_unit" Info
			# Source: ApiHelperV1.php getSessionDetails adds 'current_unit' array
			unit_info <- list(
				unit_id = NA_integer_,
				unit_type = NA_character_,
				unit_description = NA_character_,
				unit_session_id = NA_integer_
			)
			
			if (!is.null(res$current_unit)) {
				unit_info$unit_id <- as.integer(res$current_unit$id)
				unit_info$unit_type <- as.character(res$current_unit$type)
				unit_info$unit_description <- as.character(res$current_unit$description)
				unit_info$unit_session_id <- as.integer(res$current_unit$session_id)
				res$current_unit <- NULL # Remove nested list to allow flattening
			}
			
			# 3. Clean NULLs (Convert to NA)
			# Iterate over remaining fields; if NULL, set to NA
			res_clean <- lapply(res, function(x) if (is.null(x)) NA else x)
			
			# 4. Combine session data with unit data
			combined <- c(res_clean, unit_info)
			as.data.frame(combined, stringsAsFactors = FALSE)
			
		}, error = function(e) {
			warning(sprintf("⚠️ Failed to fetch session '%s': %s", code, e$message))
			return(NULL)
		})
	}
	
	# Loop over codes (handling single or multiple inputs)
	results_list <- lapply(session_codes, fetch_one)
	
	# Bind into a single data frame
	df <- dplyr::bind_rows(results_list)
	
	if (nrow(df) == 0) {
		return(dplyr::tibble())
	}
	
	# 5. Type Conversion (Make it tidy)
	# Timestamps coming from API are strings or NAs
	df <- df %>%
		dplyr::mutate(
			created = as.POSIXct(created),
			ended = as.POSIXct(ended),
			last_access = as.POSIXct(last_access),
			# Convert integers/logicals safely
			id = as.integer(id),
			position = as.integer(position),
			testing = as.logical(testing),
			deactivated = as.logical(deactivated)
		) %>%
		dplyr::as_tibble()
	
	return(df)
}