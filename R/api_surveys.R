#' List Surveys
#' 
#' Returns a list of all surveys owned by the user.
#' 
#' @param name_pattern Optional. Filter surveys by name (partial match).
#' @return A tibble of surveys (id, name, created, modified, results_table).
#' @importFrom dplyr bind_rows as_tibble mutate
#' @export
formr_surveys <- function(name_pattern = NULL) {
	query <- list()
	if (!is.null(name_pattern)) query$name <- name_pattern
	
	# Source: ApiHelperV1.php surveys (GET)
	res <- formr_api_request("surveys", query = query)
	
	if (length(res) == 0) {
		message("ℹ No surveys found.")
		return(dplyr::tibble())
	}
	
	dplyr::bind_rows(res) %>% 
		dplyr::mutate(
			created = as.POSIXct(created),
			modified = as.POSIXct(modified),
			id = as.integer(id)
		) %>%
		dplyr::as_tibble()
}

#' Get Survey Structure (Items)
#' 
#' Retrieves the item table for a survey. Handles complex nested JSON structures
#' by converting attributes into list-columns.
#' 
#' @param survey_name The name of the survey.
#' @return A tibble containing the items.
#' @importFrom dplyr bind_rows as_tibble
#' @export
formr_survey_structure <- function(survey_name) {
	# 1. Fetch Data
	# Source: ApiHelperV1.php surveys (GET /surveys/{name})
	res <- formr_api_request(
		endpoint = paste0("surveys/", survey_name), 
		method = "GET"
	)
	
	# 2. Validation
	if (is.null(res$items) || length(res$items) == 0) {
		warning(sprintf("⚠️ Survey '%s' exists but has no items.", survey_name))
		return(dplyr::tibble())
	}
	
	# 3. Helper to clean a single item
	# This prevents "row explosion" by wrapping vectors/lists in a list()
	process_item <- function(x) {
		
		# List of fields that are known to be nested or vector-heavy
		# We must wrap these in list() so they occupy 1 cell in the table
		complex_fields <- c(
			"input_attributes", 
			"parent_attributes", 
			"allowed_classes", 
			"choices", 
			"val_errors", 
			"val_warnings"
		)
		
		for (field in complex_fields) {
			if (!is.null(x[[field]])) {
				x[[field]] <- list(x[[field]])
			} else {
				x[[field]] <- list(NULL) # Ensure column exists as list-column
			}
		}
		
		# Convert NULLs to NAs (scalars only) to keep tibble happy
		x <- lapply(x, function(val) {
			if (is.null(val)) return(NA)
			return(val)
		})
		
		return(tibble::as_tibble(x))
	}
	
	# 4. Iterate and Bind
	# res$items is a named list. We remove names to avoid row-name issues.
	item_list <- unname(res$items)
	
	# Process each item individually
	clean_list <- lapply(item_list, process_item)
	
	# Combine
	df <- dplyr::bind_rows(clean_list)
	
	return(df)
}

#' Upload/Update Survey
#' 
#' Uploads a survey structure.
#' 
#' @param file_path Path to a local file.
#' @param survey_name Optional name.
#' @param google_sheet_url Google Sheet URL.
#' @export
formr_upload_survey <- function(file_path = NULL, survey_name = NULL, google_sheet_url = NULL) {
	
	if (is.null(survey_name)) {
		if (!is.null(file_path)) {
			survey_name <- tools::file_path_sans_ext(basename(file_path))
			message(sprintf("ℹ Name not provided. Defaulting survey_name to '%s'", survey_name))
		} else {
			stop("You must provide 'survey_name' if you are not providing a local 'file_path'.")
		}
	}
	
	body <- list()
	
	if (!is.null(google_sheet_url)) {
		body$google_sheet <- google_sheet_url
	} else if (!is.null(file_path)) {
		if (!file.exists(file_path)) stop("File not found: ", file_path)
		body$file <- httr::upload_file(file_path)
	} else {
		stop("You must provide either 'file_path' or 'google_sheet_url'.")
	}
	
	res <- formr_api_request(
		endpoint = paste0("surveys/", survey_name),
		method = "POST",
		body = body,
		# Force multipart if we have a file, otherwise json
		encode = if (!is.null(file_path)) "multipart" else "json"
	)
	
	message(sprintf("✅ Survey '%s' processed successfully.", res$name))
	
	if (!is.null(res$logs) && length(res$logs) > 0) {
		cat("\n📋 Server Logs:\n")
		logs <- unlist(res$logs)
		clean_logs <- gsub("<[^>]+>", "", logs)
		cat(paste0("  • ", clean_logs, collapse = "\n"), "\n")
	}
	
	invisible(res)
}