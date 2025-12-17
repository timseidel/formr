#' Backup a study
#'
#' Downloads the full run structure, all survey items, attached files, and results.
#' Saves everything into a structured folder.
#'
#' @param run_name Name of the run/study.
#' @param save_path Local folder to save data (defaults to run_name).
#' @param overwrite Logical. Overwrite existing files?
#' @export
formr_backup_run <- function(run_name, save_path = NULL, overwrite = FALSE) {
	
	if (is.null(save_path)) save_path <- run_name
	
	# 1. Directory Creation & Safety Check
	if (dir.exists(save_path)) {
		if (!overwrite) {
			stop(sprintf("Directory '%s' already exists. Set overwrite = TRUE to proceed.", save_path))
		}
	} else {
		# Attempt to create directory and STOP if it fails (e.g., permission errors)
		created <- dir.create(save_path, showWarnings = TRUE, recursive = TRUE)
		if (!created) {
			stop(sprintf("CRITICAL ERROR: Could not create directory '%s'. Check your permissions or path.", save_path))
		}
	}
	
	message(sprintf(" Backing up study '%s' to '%s'...", run_name, save_path))
	
	# 2. Run Structure (JSON)
	tryCatch({
		struct <- formr_run_structure(run_name)
		jsonlite::write_json(struct, file.path(save_path, "run_structure.json"), pretty = TRUE, auto_unbox = TRUE)
		
		# 3. Surveys (Using Shared Helper)
		.sync_server_surveys(struct, dir = save_path)
		
	}, error = function(e) warning("Failed to download run structure: ", e$message))
	
	# 4. Files (Using Shared Helper)
	.sync_server_files(run_name, dir = save_path)
	
	# 5. Results
	message("  Downloading results...")
	tryCatch({
		results <- formr_results(run_name)
		saveRDS(results, file = file.path(save_path, "results.rds"))
		message("   [SUCCESS] Results saved to results.rds")
	}, error = function(e) warning("Failed to download results: ", e$message))
	
	message("[SUCCESS] Backup complete.")
}