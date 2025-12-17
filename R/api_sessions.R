#' List Sessions in a Run
#' 
#' Returns a tidy data frame of sessions. Automatically enriches active sessions 
#' with detailed data (like unit_id, user_id) while keeping the list fast for empty sessions.
#' 
#' @param run_name Name of the run.
#' @param active Filter: TRUE for ongoing, FALSE for finished, NULL for all.
#' @param testing Filter: TRUE for test sessions, FALSE for real users, NULL for all.
#' @param limit Pagination limit (default 100).
#' @param offset Pagination offset (default 0).
#' @param detailed Logical. If TRUE (default), fetches extra details for active users.
#' @return A combined tibble of session states and details.
#' @importFrom dplyr bind_rows mutate left_join select distinct
#' @export
formr_sessions <- function(run_name, active = NULL, testing = NULL, limit = 1000, offset = 0, detailed = TRUE) {
	
	# 1. Fetch the Basic List (The "Phonebook")
	query <- list(limit = limit, offset = offset)
	if (!is.null(active)) query$active <- if(active) 1 else 0
	if (!is.null(testing)) query$testing <- if(testing) 1 else 0
	
	res <- formr_api_request(paste0("runs/", run_name, "/sessions"), query = query)
	
	if (length(res) == 0) {
		message(sprintf("[INFO] No sessions found for run '%s'.", run_name))
		return(dplyr::tibble())
	}
	
	# Basic Tidy Table
	list_df <- dplyr::bind_rows(res) %>%
		dplyr::mutate(
			created = as.POSIXct(.data$created),
			last_access = as.POSIXct(.data$last_access),
			ended = as.POSIXct(.data$ended),
			testing = as.logical(.data$testing),
			position = as.integer(.data$position)
		)
	
	# If we don't want details, or there are no active users, return early
	if (!detailed || nrow(list_df) == 0) return(list_df)
	
	# 3. Fetch Details (The "Dossier")
	message(sprintf(" Fetching details for %d active participants...", nrow(list_df)))
	details_df <- formr_session_details(run_name, list_df$session)
	
	if (nrow(details_df) == 0) return(list_df)
	
	# 4. Smart Merge
	
	new_cols <- setdiff(names(details_df), names(list_df))
	# Always keep 'session' for the join
	cols_to_merge <- c("session", new_cols)
	
	final_df <- list_df %>%
		dplyr::left_join(
			details_df %>% dplyr::select(dplyr::all_of(cols_to_merge)), 
			by = "session"
		)
	
	return(final_df)
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
		message(sprintf("[SUCCESS] Successfully created %d session(s).", count_created))
		# Print the first few created codes
		if(length(res$sessions) > 0) {
			shown <- head(res$sessions, 5)
			cat(paste0("   Codes: ", paste(shown, collapse = ", ")))
			if(length(res$sessions) > 5) cat(" ...")
			cat("\n")
		}
	}
	
	if (count_failed > 0) {
		warning(sprintf("[WARNING] Failed to create %d session(s).", count_failed))
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
			warning(sprintf("[WARNING] Failed to perform action on '%s': %s", code, e$message))
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
		message(sprintf("[SUCCESS] Action '%s' successfully performed on all %d session(s).", action, total_count))
	} else if (success_count > 0) {
		message(sprintf("[INFO] Action '%s' performed on %d/%d session(s). (See warnings for failures)", action, success_count, total_count))
	} else {
		warning(sprintf("[FAILED] Action '%s' failed for all %d session(s).", action, total_count))
	}
	
	invisible(results)
}

#' Get details for specific sessions (Helper)
#' 
#' Fetches detailed state for a list of session codes.
#' Handles schema mismatches and missing data robustly.
#' 
#' @param run_name Name of the run
#' @param session_codes Vector of session IDs
#' @export
formr_session_details <- function(run_name, session_codes) {
	
	fetch_one <- function(code) {
		tryCatch({
			res <- formr_api_request(
				endpoint = paste0("runs/", run_name, "/sessions/", code), 
				method = "GET"
			)
			
			if (!is.list(res)) return(NULL)
			
			# --- ROBUST NORMALIZATION ---
			# Ensures every field has exactly Length 1
			res_safe <- lapply(res, function(x) {
				# Case 1: Handle NULLs/Empty (force to NA)
				if (is.null(x) || length(x) == 0) return(NA)
				
				# Case 2: Handle Nested Lists (wrap in list() to keep as one object)
				# This fixes 'current_unit' breaking the table
				if (is.list(x) || length(x) > 1) return(list(x))
				
				# Case 3: Standard atomic values
				return(x)
			})
			
			# Use tibble::as_tibble, it handles list-columns better than as.data.frame
			dplyr::as_tibble(res_safe)
			
		}, error = function(e) {
			# Use paste0 for safer error printing
			safe_msg <- paste0("[WARNING] Failed to fetch details for '", code, "': ", e$message)
			warning(safe_msg, call. = FALSE)
			return(NULL)
		})
	}
	
	# Run loop
	results_list <- lapply(session_codes, fetch_one)
	df <- dplyr::bind_rows(results_list)
	
	if (nrow(df) == 0) return(dplyr::tibble())
	
	# Standardize types
	# Note: We must check if columns exist because API might omit them
	if ("created" %in% names(df)) df$created <- as.POSIXct(df$created)
	if ("ended" %in% names(df)) df$ended <- as.POSIXct(df$ended)
	if ("last_access" %in% names(df)) df$last_access <- as.POSIXct(df$last_access)
	if ("id" %in% names(df)) df$id <- as.integer(df$id)
	if ("position" %in% names(df)) df$position <- as.integer(df$position)
	
	return(df)
}