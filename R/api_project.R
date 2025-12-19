#' Initialize a Formr Project
#' Scaffolds folder structure and downloads current state.
#' @param run_name Name of the run.
#' @param dir Local directory (default ".").
#' @export
formr_init_project <- function(run_name, dir = ".") {
	dirs <- c("surveys", "files", "css", "js")
	for (d in dirs) {
		if (!dir.exists(file.path(dir, d))) dir.create(file.path(dir, d), recursive = TRUE)
	}
	
	# Create .formrignore if missing
	if (!file.exists(file.path(dir, ".formrignore"))) {
		writeLines(c(".git", ".Rproj.user", "*.Rproj", ".DS_Store", "backup/", "results.rds"), file.path(dir, ".formrignore"))
	}
	
	# Initial Pull (No prompt needed for initialization)
	formr_pull_project(run_name, dir, prompt = FALSE)
}

#' Pull Project from Server (Manual Sync)
#' Overwrites local files with Server state.
#' @param run_name Name of the run.
#' @param dir Local directory (default ".").
#' @param prompt Logical. If TRUE (default), asks for confirmation before overwriting.
#' @export
formr_pull_project <- function(run_name, dir = ".", prompt = TRUE) {
	
	if (prompt) {
		cat(sprintf("[WARNING]  WARNING: You are about to overwrite local files in '%s' with data from run '%s'.\n", normalizePath(dir, mustWork = FALSE), run_name))
		cat("   Any local changes that have not been pushed to the server will be LOST.\n")
		
		response <- readline(prompt = "   Are you sure you want to proceed? (y/n): ")
		if (tolower(trimws(response)) != "y") {
			message("[FAILED] Operation cancelled.")
			return(invisible(FALSE))
		}
	}
	
	message("Pulling changes from Server...")
	
	struct <- NULL
	
	# 1. Structure
	tryCatch({
		struct <- formr_run_structure(run_name)
		jsonlite::write_json(struct, file.path(dir, "run_structure.json"), pretty = TRUE, auto_unbox = TRUE)
		message("[SUCCESS] Structure downloaded.")
	}, error = function(e) warning("Failed to pull structure: ", e$message))
	
	# 2. Settings (Specific to Project Management, usually skipped in simple backups)
	tryCatch({
		settings <- formr_run_settings(run_name)
		
		# CSS/JS extraction
		if (!is.null(settings$custom_css)) {
			writeLines(settings$custom_css, file.path(dir, "css", "custom.css")); settings$custom_css <- "" 
		}
		if (!is.null(settings$custom_js)) {
			writeLines(settings$custom_js, file.path(dir, "js", "custom.js")); settings$custom_js <- ""
		}
		
		# Cleanup
		read_only <- c("id", "link", "created", "modified", "json_jwt")
		for (ro in read_only) settings[[ro]] <- NULL
		jsonlite::write_json(settings, file.path(dir, "run_settings.json"), pretty = TRUE, auto_unbox = TRUE)
		message("[SUCCESS] Settings downloaded.")
	}, error = function(e) warning("Failed to pull settings: ", e$message))
	
	# 3. Surveys (Using Shared Helper)
	if (!is.null(struct)) {
		.sync_server_surveys(struct, dir)
	}
	
	# 4. Files (Using Shared Helper)
	.sync_server_files(run_name, dir)
	
	message("[SUCCESS] Project files updated from server.")
}

#' Start Project Watcher (Push Only)
#' Monitors local files and pushes changes to formr immediately.
#' @param run_name Name of the run.
#' @param dir Local directory (default ".").
#' @param interval Seconds between checks (default 2).
#' @export
formr_watch_project <- function(run_name, dir = ".", interval = 2) {
	if (!dir.exists(dir)) stop("Directory not found. Run formr_init_project() first.")
	
	message(sprintf("[INFO] Watching '%s' -> Run '%s'", normalizePath(dir), run_name))
	message("   (Local edits are Pushed to Server. Press Esc to stop)")
	
	# Initial State
	last_state <- get_project_state(dir)
	
	tryCatch(
		{
			while (TRUE) {
				Sys.sleep(interval)
				
				current_state <- get_project_state(dir)
				changes <- detect_changes(last_state, current_state)
				
				if (length(changes$added) > 0 || length(changes$modified) > 0 || length(changes$deleted) > 0) {
					# Sync Logic
					handle_project_changes(run_name, dir, changes)
					
					# Update State
					last_state <- current_state
				}
			}
		},
		interrupt = function(i) {
			message("\n[INFO] Watcher stopped.")
		}
	)
}

# --- INTERNAL HELPERS ---

#' Get recursive file state
#' @noRd
get_project_state <- function(dir) {
    all_files <- list.files(dir, recursive = TRUE, full.names = FALSE, all.files = TRUE)
    ignores <- read_ignore_file(dir)
    files_to_track <- Filter(function(f) !is_ignored(f, ignores), all_files)
    info <- file.info(file.path(dir, files_to_track))
    structure(as.numeric(info$mtime), names = files_to_track)
}

#' Read .formrignore
#' @noRd
read_ignore_file <- function(dir) {
    f <- file.path(dir, ".formrignore")
    if (file.exists(f)) {
        lines <- readLines(f, warn = FALSE)
        lines <- lines[trimws(lines) != "" & !startsWith(trimws(lines), "#")]
        return(lines)
    }
    return(c())
}

#' Check ignore patterns (Fixed for nested files)
#' @noRd
is_ignored <- function(file, patterns) {
    # 1. Always ignore these specific system files
    if (file %in% c(".formrignore", ".git", ".Rhistory")) {
        return(TRUE)
    }

    for (p in patterns) {
        # 2. Handle Directory Ignores (e.g. "backup/")
        if (endsWith(p, "/")) {
            if (startsWith(file, p)) {
                return(TRUE)
            }
        }

        # 3. Handle File Ignores (e.g. ".DS_Store" or "*.png")
        # If the pattern has no slashes, we check if the filename matches (ignoring the folder path)
        if (!grepl("/", p)) {
            if (grepl(glob2rx(p), basename(file))) {
                return(TRUE)
            }
        }

        # 4. Handle Path Ignores (e.g. "files/secret.csv")
        # Check the full path against the pattern
        if (grepl(glob2rx(p), file)) {
            return(TRUE)
        }
    }

    return(FALSE)
}

#' Detect Changes
#' @noRd
detect_changes <- function(old_state, new_state) {
    old_files <- names(old_state)
    new_files <- names(new_state)

    added <- setdiff(new_files, old_files)
    deleted <- setdiff(old_files, new_files)

    common <- intersect(new_files, old_files)
    modified <- common[new_state[common] > old_state[common]]

    list(added = added, modified = modified, deleted = deleted)
}

#' Router: Handle changes
#' @noRd
handle_project_changes <- function(run_name, dir, changes) {
	to_process <- c(changes$added, changes$modified)
	
	# 1. SETTINGS & CSS/JS
	if (any(grepl("^(css/|js/|run_settings\\.json)", to_process))) {
		message(" Syncing Settings...")
		sync_run_settings(run_name, dir)
	}
	
	# 2. SURVEYS
	survey_files <- grep("^surveys/", to_process, value = TRUE)
	for (f in survey_files) {
		s_name <- tools::file_path_sans_ext(basename(f))
		
		# Check for spaces or other URL-unsafe characters
		if (grepl("[^a-zA-Z0-9_-]", s_name)) {
			suggested_name <- gsub("[^a-zA-Z0-9_-]", "_", s_name)
			
			message("[WARNING]  SKIPPED: '", basename(f), "'")
			message("   Reason: Survey names cannot contain spaces or special characters.")
			message("   Action: Please rename the file locally to '", suggested_name, ".xlsx'")
			next 
		}
		
		message("[INFO] Syncing Survey: ", s_name)
		
		tryCatch(
			{
				formr_upload_survey(file_path = file.path(dir, f), survey_name = s_name)
				message("   [SUCCESS] Upload success")
			},
			error = function(e) {
				message("   [FAILED] Upload failed: ", e$message)
			}
		)
	}
	
	# 3. UPLOAD FILES
	asset_files <- grep("^files/", to_process, value = TRUE)
	if (length(asset_files) > 0) {
		message("Vm  Uploading ", length(asset_files), " file(s)...")
		for (f in asset_files) {
			try(formr_upload_file(run_name, file.path(dir, f)))
		}
	}
	
	# 4. DELETE FILES
	deleted_assets <- grep("^files/", changes$deleted, value = TRUE)
	if (length(deleted_assets) > 0) {
		message("Deleting ", length(deleted_assets), " file(s)...")
		
		for (del_f in deleted_assets) {
			raw_name <- basename(del_f)
			server_name <- gsub(" ", "_", raw_name)
			
			tryCatch(
				{
					formr_delete_file(run_name, server_name)
				},
				error = function(e) {
					message("   [WARNING] Could not delete '", server_name, "': ", e$message)
				}
			)
		}
	}
	
	# 5. STRUCTURE (Modified with Auto-Fix)
	if ("run_structure.json" %in% to_process) {
		message("[INFO]  Syncing Run Structure...")
		
		json_path <- file.path(dir, "run_structure.json")
		
		# The server crashes on empty objects "{}" for fields that expect strings/nulls.
		# This block reads the file, replaces ": {}" with ": null", and saves it back.
		tryCatch({
			txt <- paste(readLines(json_path, warn = FALSE), collapse = "\n")
			
			# Regex to find "key": {} and replace with "key": null
			# Matches: quote, colon, optional space, open brace, close brace
			clean_txt <- gsub('":\\s*\\{\\}', '": null', txt)
			
			# Only write if changes were needed
			if (txt != clean_txt) {
				message("   [AUTO-FIX] Converting empty objects '{}' to 'null' for compatibility.")
				writeLines(clean_txt, json_path)
			}
		}, error = function(e) {
			warning("Failed to auto-clean JSON: ", e$message)
		})
		
		try(formr_run_structure(run_name, structure_json_path = json_path))
	}
}

#' Helper: Merge Settings + CSS + JS (with type safety)
#' @noRd
sync_run_settings <- function(run_name, dir) {
	settings_path <- file.path(dir, "run_settings.json")
	if (!file.exists(settings_path)) return()
	
	settings <- jsonlite::read_json(settings_path)
	
	# Fix Types (PHP boolean issue)
	settings <- lapply(settings, function(x) if (is.logical(x)) as.integer(x) else x)
	
	# Inject CSS
	css_file <- file.path(dir, "css", "custom.css")
	if (file.exists(css_file)) {
		# Check for "Magic" behavior: Are we overwriting existing JSON settings?
		if (!is.null(settings$custom_css) && nzchar(settings$custom_css)) {
			message("[INFO]  Overriding 'custom_css' in settings with content from css/custom.css")
		}
		settings$custom_css <- paste(readLines(css_file, warn=FALSE), collapse="\n")
	}
	
	# Inject JS
	js_file <- file.path(dir, "js", "custom.js")
	if (file.exists(js_file)) {
		# Check for "Magic" behavior: Are we overwriting existing JSON settings?
		if (!is.null(settings$custom_js) && nzchar(settings$custom_js)) {
			message("[INFO]  Overriding 'custom_js' in settings with content from js/custom.js")
		}
		settings$custom_js <- paste(readLines(js_file, warn=FALSE), collapse="\n")
	}
	
	tryCatch({
		formr_run_settings(run_name, settings)
	}, error = function(e) warning("Failed to sync settings: ", e$message))
}

#' Sync Surveys from Structure to Local
#' Downloads Excel tables for all surveys defined in the structure.
#' @noRd
.sync_server_surveys <- function(struct, dir) {
	if (is.null(struct) || is.null(struct$units)) return()
	
	message("[INFO] Syncing survey tables...")
	
	# Ensure folder exists
	survey_dir <- file.path(dir, "surveys")
	if (!dir.exists(survey_dir)) dir.create(survey_dir, recursive = TRUE)
	
	count <- 0
	
	for (unit in struct$units) {
		if (identical(unit$type, "Survey")) {
			survey_name <- NULL
			if (!is.null(unit$description) && nzchar(unit$description)) {
				survey_name <- unit$description
			}
			if (is.null(survey_name) && is.list(unit$survey_data)) {
				if (!is.null(unit$survey_data$name)) survey_name <- unit$survey_data$name
			}
			
			if (!is.null(survey_name)) {
				dest <- file.path(survey_dir, paste0(survey_name, ".xlsx"))
				tryCatch({
					path <- formr_survey_structure(
						survey_name = survey_name, 
						format = "xlsx", 
						file_path = dest
					)
					if (!is.null(path)) count <- count + 1
				}, error = function(e) {
					message("   [WARNING] Failed to download '", survey_name, "': ", e$message)
				})
			}
		}
	}
	message(sprintf("[SUCCESS] Downloaded %d survey table(s).", count))
}

#' Sync Assets/Files from Server to Local
#' @noRd
.sync_server_files <- function(run_name, dir) {
	message("[INFO] Syncing assets/files...")
	
	# Ensure parent directory exists before proceeding
	if (!dir.exists(dir)) {
		warning("Skipping file sync: Parent directory '", dir, "' does not exist.")
		return()
	}
	
	tryCatch({
		files_list <- formr_api_request(endpoint = paste0("runs/", run_name, "/files"))
		
		if (length(files_list) > 0) {
			files_dir <- file.path(dir, "files")
			
			# Strict check for subfolder creation
			if (!dir.exists(files_dir)) {
				if (!dir.create(files_dir, recursive = TRUE)) {
					warning("   [WARNING] Could not create 'files' folder. Skipping downloads.")
					return()
				}
			}
			
			f_count <- 0
			for (f in files_list) {
				safe_name <- gsub(" ", "_", f$name)
				dest <- file.path(files_dir, safe_name)
				
				tryCatch({
					download.file(f$url, dest, mode = "wb", quiet = TRUE)
					f_count <- f_count + 1
				}, error = function(e) message("   [WARNING] Failed to download file '", f$name, "'"))
			}
			message(sprintf("[SUCCESS] Downloaded %d file(s).", f_count))
		} else {
			message("   (No files found on server)")
		}
	}, error = function(e) warning("Failed to sync files: ", e$message))
}
