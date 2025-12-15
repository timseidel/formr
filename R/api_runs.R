#' List all runs
#' 
#' Returns a data frame of all runs accessible to the user, including status flags
#' and timestamps.
#' 
#' @return A data.frame containing run details: id, name, title, public (bool),
#' cron_active (bool), locked (bool), created (POSIXct), modified (POSIXct).
#' @importFrom dplyr bind_rows mutate
#' @export
formr_runs <- function() {
	raw <- formr_api_request("runs", api_version = "v1")
	
	if (length(raw) == 0) return(data.frame())
	
	dplyr::bind_rows(raw) %>%
		dplyr::mutate(
			created = as.POSIXct(.data$created),
			modified = as.POSIXct(.data$modified),
			# Convert 0/1 integers to logical booleans for clarity
			public = as.logical(.data$public),
			cron_active = as.logical(.data$cron_active),
			locked = as.logical(.data$locked)
		)
}

#' Create a new run
#' 
#' Creates one or more new runs on the server. Prints a confirmation message 
#' with the public link for each.
#' 
#' @param name A character vector of names for the new runs (must be unique).
#' @return Invisibly returns a data frame containing the `name` and `link` of the created runs.
#' @importFrom dplyr bind_rows
#' @export
formr_create_run <- function(name) {
	
	# Helper function to create a single run
	create_single <- function(single_name) {
		# Endpoint: POST /v1/runs/{name}
		res <- formr_api_request(paste0("runs/", single_name), method = "POST", api_version = "v1")
		
		# User-friendly feedback
		message(sprintf("✅ Success! Run '%s' created.", res$name))
		message(sprintf("🔗 Link: %s", res$link))
		
		return(res)
	}
	
	# Iterate over all provided names
	results_list <- lapply(name, create_single)
	
	# Combine results into a tidy data frame
	results_df <- dplyr::bind_rows(results_list)
	
	# Return the result invisibly so it can be assigned if needed
	invisible(results_df)
}

#' Get or Update Run Settings
#' 
#' Retrieve the settings for one or more runs as a tidy data frame, or update them
#' by providing a named list of new values.
#' 
#' @param run_name Name of the run (or a vector of names).
#' @param settings A list of settings to update (e.g., `list(public = 1, locked = TRUE)`). 
#'   If NULL, returns the current settings.
#' @return 
#'   - If `settings` is NULL: A data.frame/tibble with details for all requested runs.
#'   - If `settings` is provided: Invisibly returns TRUE on success.
#' @importFrom dplyr bind_rows mutate
#' @export
formr_run_settings <- function(run_name, settings = NULL) {
	
	# --- Helper function to process a single run ---
	process_single_run <- function(single_name) {
		if (is.null(settings)) {
			# GET: Fetch and clean
			raw <- formr_api_request(paste0("runs/", single_name), api_version = "v1")
			
			# Convert to tibble
			df <- dplyr::bind_rows(raw) %>%
				dplyr::mutate(
					created = as.POSIXct(.data$created),
					modified = as.POSIXct(.data$modified),
					locked = as.logical(.data$locked),
					cron_active = as.logical(.data$cron_active),
					use_material_design = as.logical(.data$use_material_design),
					public = as.integer(.data$public)
				)
			return(df)
			
		} else {
			# PATCH: Update settings
			formr_api_request(paste0("runs/", single_name), method = "PATCH", body = settings, api_version = "v1")
			message(sprintf("✅ Settings updated successfully for run '%s'.", single_name))
			return(NULL)
		}
	}
	
	# --- Main Logic: Iterate over all provided names ---
	
	if (is.null(settings)) {
		# GET Mode: Apply function to all names and stack the resulting dataframes
		all_runs_data <- dplyr::bind_rows(lapply(run_name, process_single_run))
		return(all_runs_data)
		
	} else {
		# PATCH Mode: Apply function to all names (output is printed messages)
		# We use invisible() to suppress the list of NULLs returned by lapply
		invisible(lapply(run_name, process_single_run))
		return(invisible(TRUE))
	}
}

#' Get or Update Run Structure (Run Units)
#' 
#' Export the current run structure as a list (GET) or replace it by importing 
#' a JSON file (PUT).
#' 
#' @param run_name Name of the run.
#' @param structure_json_path Optional path to a JSON file to IMPORT structure. 
#'   If NULL, EXPORTS current structure.
#' @return 
#'   - GET: A list containing the run structure.
#'   - PUT: Invisibly returns TRUE on success.
#' @export
formr_run_structure <- function(run_name, structure_json_path = NULL) {
	
	if (is.null(structure_json_path)) {
		# --- GET Mode: Export Structure ---
		return(formr_api_request(
			endpoint = paste0("runs/", run_name, "/structure"), 
			method = "GET"
		))
		
	} else {
		# --- PUT Mode: Import Structure ---
		
		if (!file.exists(structure_json_path)) {
			stop("File not found: ", structure_json_path)
		}
		
		# 1. Read and Validate JSON locally
		json_content <- paste(readLines(structure_json_path, warn = FALSE), collapse = "\n")
		
		if (!jsonlite::validate(json_content)) {
			stop("The provided file contains invalid JSON. Please check syntax.")
		}
		
		# 2. Send PUT Request with encode="raw"
		res <- formr_api_request(
			endpoint = paste0("runs/", run_name, "/structure"),
			method = "PUT",
			body = json_content,
			encode = "raw"
		)
		
		# 3. Success Feedback
		success_msg <- "Structure imported successfully."
		if (is.list(res) && !is.null(res$statusText)) {
			success_msg <- res$statusText
		}
		
		message(sprintf("✅ %s", success_msg))
		return(invisible(TRUE))
	}
}