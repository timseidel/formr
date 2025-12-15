#' Backup a study
#'
#' Downloads the full run structure, all survey items, and results.
#' Saves everything into a structured folder.
#'
#' @param study_name Name of the run/study.
#' @param save_path Local folder to save data (defaults to study_name).
#' @param overwrite Logical. Overwrite existing files?
#' @export
formr_backup_study <- function(study_name, save_path = study_name, overwrite = FALSE) {
	
	if (dir.exists(save_path) && !overwrite) {
		stop(sprintf("Directory '%s' already exists. Set overwrite = TRUE to proceed.", save_path))
	}
	dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
	
	message(sprintf("📦 Backing up study '%s' to '%s'...", study_name, save_path))
	
	# 1. Run Structure (JSON)
	tryCatch({
		formr_run_structure(study_name, file = file.path(save_path, "run_structure.json"))
	}, error = function(e) warning("Failed to download run structure: ", e$message))
	
	# 2. Surveys (Items as JSON/CSV)
	# We extract survey names from the structure to know what to fetch
	structure <- formr_run_structure(study_name)
	survey_units <- Filter(function(u) u$type == "Survey", structure$units)
	
	if (length(survey_units) > 0) {
		survey_names <- unique(sapply(survey_units, function(u) u$survey_data$name))
		formr_backup_surveys(survey_names, save_path = file.path(save_path, "surveys"))
	}
	
	# 3. Results (RDS/CSV)
	message("⬇️  Downloading results...")
	results <- formr_results(study_name)
	saveRDS(results, file = file.path(save_path, "results.rds"))
	
	# 4. Files
	message("⬇️  Downloading attached files...")
	formr_backup_files(study_name, save_path = file.path(save_path, "files"), overwrite = overwrite)
	
	message("✅ Backup complete.")
}

#' Backup Surveys
#' 
#' Downloads item tables for a list of surveys.
#' 
#' @param survey_names Vector of survey names.
#' @param save_path Directory to save files.
#' @export
formr_backup_surveys <- function(survey_names, save_path = "surveys") {
	dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
	
	for (name in survey_names) {
		message(sprintf("   - Survey: %s", name))
		try({
			items <- formr_survey_structure(name)
			# Save as RDS (preserves types/nested lists) and CSV (readable backup)
			saveRDS(items, file.path(save_path, paste0(name, "_items.rds")))
			
			# Flatten for CSV if possible, otherwise just skip
			try(readr::write_csv(items, file.path(save_path, paste0(name, "_items.csv"))), silent = TRUE)
		})
	}
}

#' Backup uploaded files
#' 
#' Downloads all files attached to a run.
#' 
#' @param run_name Name of the run.
#' @param save_path Local directory.
#' @param overwrite Overwrite existing files?
#' @export
formr_backup_files <- function(run_name, save_path = "files", overwrite = FALSE) {
	dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
	
	files <- formr_files(run_name)
	
	if (nrow(files) == 0) {
		message("   (No files found)")
		return(invisible(NULL))
	}
	
	for (i in 1:nrow(files)) {
		f <- files[i, ]
		local_path <- file.path(save_path, f$name)
		
		if (file.exists(local_path) && !overwrite) {
			next # Skip
		}
		
		# Download using the public URL (if accessible) or API endpoint if needed
		# Assuming `url` in file object is accessible
		tryCatch({
			utils::download.file(f$url, destfile = local_path, mode = "wb", quiet = TRUE)
			message(sprintf("   - Downloaded: %s", f$name))
		}, error = function(e) {
			warning(sprintf("Failed to download '%s': %s", f$name, e$message))
		})
	}
}

#### WATCHER AND SYNCH FUNCTIONALITY

#' Synchronize a local folder with a run (Smart Sync)
#' 
#' Syncs local files to the server.
#' - Uploads new files.
#' - Deletes missing files.
#' - UPDATES files if the local version is newer than the server version.
#' 
#' @param run_name Name of the run.
#' @param folder_path Local directory to sync.
#' @param tolerance Seconds of difference allowed before considering a file "changed" (default 2s).
#' @param dry_run If TRUE, only prints changes without executing them.
#' @export
formr_sync_folder <- function(run_name, folder_path, tolerance = 2, dry_run = FALSE) {
	
	if (!dir.exists(folder_path)) stop("Local folder not found: ", folder_path)
	
	# --- 1. Get Server State ---
	server_df <- formr_files(run_name)
	
	# Handle empty server state
	if (nrow(server_df) == 0) {
		server_map <- list()
	} else {
		# Create a named list for quick lookup: name -> modified_time
		# We ensure server time is numeric (seconds since epoch) for easy comparison
		server_map <- as.list(as.numeric(server_df$modified))
		names(server_map) <- server_df$name
	}
	
	# --- 2. Get Local State ---
	local_files_full <- list.files(folder_path, recursive = FALSE, full.names = TRUE)
	local_files_names <- basename(local_files_full)
	
	# Get modification times (mtime)
	local_info <- file.info(local_files_full)
	local_mtimes <- as.numeric(local_info$mtime)
	names(local_mtimes) <- local_files_names
	
	# --- 3. Calculate Differences ---
	
	# A. Files to Delete (On server, but not local)
	to_delete <- setdiff(names(server_map), local_files_names)
	
	# B. Files to Upload (New OR Modified)
	to_upload <- c()
	
	for (f_name in local_files_names) {
		is_new <- ! (f_name %in% names(server_map))
		
		if (is_new) {
			# It's a new file
			to_upload <- c(to_upload, f_name)
		} else {
			# It exists on both; check timestamps
			server_time <- server_map[[f_name]]
			local_time  <- local_mtimes[[f_name]]
			
			# Logic: If local file is significantly NEWER than server file
			# (We use tolerance to ignore tiny clock skews)
			if ((local_time - server_time) > tolerance) {
				to_upload <- c(to_upload, f_name)
			}
		}
	}
	
	# --- 4. Execute / Report ---
	
	if (length(to_upload) == 0 && length(to_delete) == 0) {
		return(invisible(FALSE)) # Silent return if nothing happened
	}
	
	message(sprintf("--- Syncing '%s' ---", folder_path))
	
	if (dry_run) {
		if (length(to_upload) > 0) cat("Dry Run [Upload/Update]:", paste(to_upload, collapse=", "), "\n")
		if (length(to_delete) > 0) cat("Dry Run [Delete]:", paste(to_delete, collapse=", "), "\n")
		return(invisible(TRUE))
	}
	
	# Perform Deletions
	if (length(to_delete) > 0) {
		formr_delete_file(run_name, to_delete)
	}
	
	# Perform Uploads (This handles both new files and overwrites)
	if (length(to_upload) > 0) {
		# Reconstruct full paths
		paths_to_upload <- file.path(folder_path, to_upload)
		formr_upload_file(run_name, paths_to_upload)
	}
	
	invisible(TRUE)
}

#' Start a Folder Watcher
#' 
#' Continuously monitors a local folder and syncs changes to the formr run.
#' ⚠ BLOCKING: This function runs an infinite loop. Press Esc to stop.
#' 
#' @param run_name Name of the run.
#' @param folder_path Local directory to watch.
#' @param interval Seconds to wait between checks.
#' @export
formr_start_watcher <- function(run_name, folder_path, interval = 5) {
	message(sprintf("👀 Watching folder '%s' for run '%s'...", folder_path, run_name))
	message("Press Esc to stop.")
	
	tryCatch({
		while(TRUE) {
			# Run the sync silently
			formr_sync_folder(run_name, folder_path)
			
			# Wait before checking again
			Sys.sleep(interval)
		}
	}, interrupt = function(i) {
		message("\n🛑 Watcher stopped by user.")
	})
}


#' Get Full Participants Table
#' 
#' Combines listing and inspection to give a full table of user states.
#' 
#' @param run_name Name of the run
#' @param limit How many recent users to fetch (default 50)
#' @return A detailed tibble of recent users
formr_participants_table <- function(run_name, limit = 50) {
	
	# 1. Get the list (The Phonebook)
	# Uses the lightweight formr_sessions logic
	message("📋 Fetching participant list...")
	list_df <- formr_sessions(run_name, limit = limit)
	
	if (nrow(list_df) == 0) return(list_df)
	
	# 2. Get the details for these specific codes (The Dossier)
	# Uses your new vectorized formr_session_details
	message(sprintf("🔍 Inspecting %d participants...", nrow(list_df)))
	details_df <- formr_session_details(run_name, list_df$session)
	
	return(details_df)
}