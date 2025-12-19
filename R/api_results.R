#' Get Run Results
#'
#' Fetches results and converts them into tidy tibbles.
#'
#' @param run_name Name of the run.
#' @param surveys Optional. Vector of survey names to fetch.
#' @param session_ids Optional. Filter by specific session IDs.
#' @param item_names Optional. Filter by specific item names (columns).
#' @param join Logical. If TRUE, merges all surveys into one wide table by 'session',
#'   suffixing column names with the survey name to prevent conflicts.
#' @return A tibble (if joined or only 1 survey found) or a named list of tibbles.
#' @importFrom dplyr bind_rows as_tibble full_join rename_with
#' @importFrom readr type_convert cols
#' @importFrom purrr map reduce
#' @export
formr_results <- function(run_name,
													surveys = NULL,
													session_ids = NULL,
													item_names = NULL,
													join = FALSE) {
	# 1. Build Query
	# Source: ApiHelperV1.php handleResults
	query <- list()
	if (!is.null(surveys))
		query$surveys <- paste(surveys, collapse = ",")
	if (!is.null(session_ids))
		query$sessions <- paste(session_ids, collapse = ",")
	if (!is.null(item_names))
		query$items <- paste(item_names, collapse = ",")
	
	# 2. Fetch Data
	res <- formr_api_request(endpoint = paste0("runs/", run_name, "/results"),
													 query = query)
	
	if (length(res) == 0) {
		message("[INFO] No results found.")
		return(dplyr::tibble(session = character()))
	}
	
	# 3. Helper: Clean a Single Survey
	clean_survey_data <- function(rows) {
		if (length(rows) == 0)
			return(dplyr::tibble(session = character()))
		
		# Bind rows and auto-convert types
		df <- dplyr::bind_rows(rows)
		df <- suppressMessages(readr::type_convert(df, col_types = readr::cols()))
		return(df)
	}
	
	# 4. Process List (Result is a named list of tibbles)
	results_list <- purrr::map(res, clean_survey_data)
	
	# 5. Handle Join Logic
	if (join && length(results_list) > 1) {
		message(" Joining surveys by 'session'...")
		all_cols <- unlist(lapply(results_list, names))
		duplicates <- all_cols[duplicated(all_cols) &
													 	all_cols != "session"]
		if (length(duplicates) == 0) {
			# Safe to join without renaming!
			message("Joining surveys (no column conflicts detected)...")
			joined_df <- purrr::reduce(results_list, dplyr::full_join, by = "session")
			return(joined_df)
		} else {
			# Pre-processing: Rename columns to {column}_{survey_name}
			# This avoids the .x .y .x.x mess entirely
			results_list_renamed <- mapply(function(df, s_name) {
				if (ncol(df) == 0)
					return(df)
				
				# We want to rename everything EXCEPT 'session' (the join key)
				# and potentially 'session_id' if you want that common.
				# Usually 'session' is the unique string code.
				cols_to_rename <- setdiff(names(df), "session")
				
				if (length(cols_to_rename) > 0) {
					# Create new names: e.g. "created" -> "created_surveyName"
					new_names <- paste0(cols_to_rename, "_", s_name)
					names(df)[match(cols_to_rename, names(df))] <- new_names
				}
				return(df)
			},
			results_list,
			names(results_list),
			SIMPLIFY = FALSE)
			message(sprintf(
				"Column conflicts detected (%s). Appending survey names...",
				paste(head(duplicates), collapse = ",")
			))
		}
		
		# Now we can join safely without suffixes because names are already unique
		joined_df <- purrr::reduce(results_list_renamed, function(x, y) {
			dplyr::full_join(x, y, by = "session")
		})
		
		return(joined_df)
	}
	
	# Case B: Only one survey exists (Auto-unwrap)
	if (length(results_list) == 1) {
		return(results_list[[1]])
	}
	
	# Case C: Return List (Default)
	return(results_list)
}