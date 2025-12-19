#' Get Run Results
#'
#' Fetches results and converts them into tidy tibbles.
#' Automatically handles randomization (shuffle) groups by pivoting them into columns.
#'
#' @param run_name Name of the run.
#' @param surveys Optional. Vector of survey names to fetch.
#' @param session_ids Optional. Filter by specific session IDs.
#' @param item_names Optional. Filter by specific item names (columns).
#' @param join Logical. If TRUE, merges all surveys into one wide table by 'session',
#'   suffixing column names with the survey name to prevent conflicts.
#' @return A tibble (if joined or only 1 survey found) or a named list of tibbles.
#' @importFrom dplyr bind_rows as_tibble full_join left_join rename_with select distinct
#' @importFrom readr type_convert cols
#' @importFrom purrr map reduce
#' @importFrom tidyr pivot_wider
#' @export
formr_results <- function(run_name,
													surveys = NULL,
													session_ids = NULL,
													item_names = NULL,
													join = FALSE) {
	# 1. Build Query
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
	
	# 4. Handle Shuffles (New Logic)
	# We extract 'shuffles' first so it doesn't interfere with the survey join loop
	shuffle_df <- NULL
	if ("shuffles" %in% names(res)) {
		raw_shuffles <- res$shuffles
		res$shuffles <- NULL # Remove from the list to process surveys separately
		
		if (length(raw_shuffles) > 0) {
			s_df <- clean_survey_data(raw_shuffles)
			
			# Transform: 
			# 1. Rename 'run_session' -> 'session' to match surveys
			# 2. Pivot 'unit_name' (shuffle name) -> columns
			if (nrow(s_df) > 0 && "unit_name" %in% names(s_df)) {
				shuffle_df <- s_df %>%
					dplyr::select(session = run_session, unit_name, group) %>%
					dplyr::distinct() %>% 
					tidyr::pivot_wider(
						names_from = "unit_name", 
						values_from = "group",
						values_fn = list(group = toString) # Handle duplicates safely
					)
			}
		}
	}
	
	# 5. Process Surveys (Result is a named list of tibbles)
	results_list <- purrr::map(res, clean_survey_data)
	
	# 6. Handle Join Logic
	if (join) {
		# 6a. Join Surveys
		if (length(results_list) > 1) {
			message(" Joining surveys by 'session'...")
			all_cols <- unlist(lapply(results_list, names))
			duplicates <- all_cols[duplicated(all_cols) & all_cols != "session"]
			
			if (length(duplicates) == 0) {
				# Safe to join without renaming
				message("Joining surveys (no column conflicts detected)...")
				joined_df <- purrr::reduce(results_list, dplyr::full_join, by = "session")
			} else {
				# Pre-processing: Rename columns to {column}_{survey_name}
				results_list_renamed <- mapply(function(df, s_name) {
					if (ncol(df) == 0) return(df)
					cols_to_rename <- setdiff(names(df), "session")
					if (length(cols_to_rename) > 0) {
						new_names <- paste0(cols_to_rename, "_", s_name)
						names(df)[match(cols_to_rename, names(df))] <- new_names
					}
					return(df)
				}, results_list, names(results_list), SIMPLIFY = FALSE)
				
				message(sprintf("Column conflicts detected (%s). Appending survey names...",
												paste(head(duplicates), collapse = ",")))
				
				joined_df <- purrr::reduce(results_list_renamed, function(x, y) {
					dplyr::full_join(x, y, by = "session")
				})
			}
		} else if (length(results_list) == 1) {
			joined_df <- results_list[[1]]
		} else {
			joined_df <- dplyr::tibble(session = character())
		}
		
		# 6b. Attach Shuffles to the Joined Result
		if (!is.null(shuffle_df)) {
			message(" Attaching shuffle groups...")
			# If we have survey data, left_join preserves survey rows and adds shuffle info.
			# If no survey data exists (empty joined_df), we start with the shuffle data.
			if (nrow(joined_df) == 0) {
				joined_df <- shuffle_df
			} else {
				joined_df <- dplyr::left_join(joined_df, shuffle_df, by = "session")
			}
		}
		
		return(joined_df)
	}
	
	# Case B: List Return
	# Add shuffles back to the list as a clean, wide tibble
	if (!is.null(shuffle_df)) {
		results_list$shuffles <- shuffle_df
	}
	
	# Auto-unwrap if only 1 element (e.g. just shuffles or just 1 survey)
	if (length(results_list) == 1) {
		return(results_list[[1]])
	}
	
	return(results_list)
}