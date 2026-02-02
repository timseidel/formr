#' Get and Process Run Results
#'
#' This is the main function for scientists. It fetches data from the API, 
#' automatically cleans types (dates/numbers), reverses items, computes scales, 
#' and joins everything into one dataframe.
#'
#' @param run_name Name of the run.
#' @param ... Filters passed to API (e.g. `surveys = c("Daily", "Intake")`, `session_ids = "..."`).
#' @param compute_scales Logical. Should scales (e.g. `extraversion`) be computed from items (e.g. `extra_1`, `extra_2`)?
#' @param join Logical. If TRUE (default), joins all surveys into one wide dataframe. 
#' @param remove_test_sessions Logical. Filter out sessions marked as testing?
#'
#' @return A processed tibble (if joined) or a list of processed tibbles.
#' @export
formr_api_results <- function(run_name, 
															..., 
															compute_scales = TRUE, 
															join = TRUE, 
															remove_test_sessions = TRUE) {
	
	# 1. Fetch Metadata
	message(sprintf("Fetching metadata for '%s'...", run_name))
	structure <- formr_api_run_structure(run_name)
	
	# 2. Prepare Test Session Filter (Hybrid Approach)
	test_session_codes <- character(0)
	
	if (remove_test_sessions) {
		# Attempt to fetch precise list from API
		tryCatch({
			# Note: We assume formr_api_sessions supports 'testing = TRUE' or returns a 'testing' col
			# If your API wrapper doesn't support the arg yet, we fetch all and filter locally
			all_sessions <- formr_api_sessions(run_name, limit = 100000)
			
			if ("testing" %in% names(all_sessions) && "session" %in% names(all_sessions)) {
				# Filter strictly for testing == 1 or TRUE
				test_sessions_df <- all_sessions[all_sessions$testing == 1 | all_sessions$testing == TRUE, ]
				test_session_codes <- unique(test_sessions_df$session)
				
				if (length(test_session_codes) > 0) {
					message(sprintf("... Identified %d test sessions via API.", length(test_session_codes)))
				}
			}
		}, error = function(e) {
			warning("Could not fetch session metadata from API. Falling back to 'XXX' pattern matching only.")
		})
	}
	
	# 3. Fetch Results
	message("Fetching results...")
	raw_results_list <- formr_api_fetch_results(run_name, ..., join = FALSE)
	
	# --- SAFE FINGERPRINTING (Identify Survey if Single DF) ---
	if (inherits(raw_results_list, "data.frame")) {
		data_cols <- names(raw_results_list)
		possible_matches <- c()
		
		if (!is.null(structure$units)) {
			for (unit in structure$units) {
				if (unit$type == "Survey" && !is.null(unit$survey_data$items)) {
					meta_items <- dplyr::bind_rows(unit$survey_data$items)
					relevant_items <- meta_items$name[ !meta_items$type %in% c("note", "submit", "block") ]
					
					overlap_count <- sum(relevant_items %in% data_cols)
					total_relevant <- length(relevant_items)
					
					if (total_relevant > 0 && (overlap_count / total_relevant) > 0.5) {
						possible_matches <- c(possible_matches, unit$survey_data$name)
					}
				}
			}
		}
		
		if (length(possible_matches) == 1) {
			guessed_name <- possible_matches[1]
			message(sprintf("... Auto-detected survey: '%s'", guessed_name))
			raw_results_list <- list(raw_results_list)
			names(raw_results_list) <- guessed_name
			
		} else if (length(possible_matches) > 1) {
			warning(sprintf("Ambiguous data! Matches: %s. Returning raw data.", paste(possible_matches, collapse = ", ")))
			return(raw_results_list)
		} else {
			warning("Could not match results to Run Structure. Returning raw data.")
			return(raw_results_list)
		}
	}
	# ---------------------------------------------------------
	
	# 4. Define Processing Pipeline
	process_single_survey <- function(df, survey_name) {
		if (nrow(df) == 0) return(df)
		
		# A. Filter Test Sessions (API + Heuristic)
		if (remove_test_sessions && "session" %in% names(df)) {
			initial_count <- nrow(df)
			
			# 1. Exclude API-confirmed test sessions
			if (length(test_session_codes) > 0) {
				df <- df[ !df$session %in% test_session_codes, ]
			}
			
			# 2. Exclude Heuristic 'XXX' (catches unsynced or legacy tests)
			df <- df[ !grepl("XXX", df$session), ]
		}
		
		# B. Get Metadata
		survey_meta <- .extract_items_for_survey(structure, survey_name)
		if (is.null(survey_meta)) return(df)
		
		# C. Process
		df <- formr_api_recognise(survey_meta, df) 
		df <- formr_api_reverse(df, survey_meta)   
		
		if (compute_scales) {
			df <- formr_api_aggregate(df, survey_meta)
		}
		
		return(df)
	}
	
	# 5. Apply & Join
	message("Processing data (types, reversals, scales)...")
	processed_list <- purrr::imap(raw_results_list, function(df, name) {
		if (name == "shuffles") return(df) 
		process_single_survey(df, name)
	})
	
	if (join) {
		message("Joining surveys...")
		return(.join_results(processed_list))
	}
	
	return(processed_list)
}

#' Lower-level API Result Fetcher
#'
#' Fetches raw results. Advanced users can use this if they want 
#' completely raw data without any type coercion or processing.
#'
#' @param run_name Name of the run.
#' @param surveys Optional character vector of survey names to filter by.
#' @param session_ids Optional character vector of session IDs to filter by.
#' @param item_names Optional character vector of item names to filter by.
#' @param join Logical. If TRUE, joins the results into a single data frame.
#' @export
formr_api_fetch_results <- function(run_name,
																		surveys = NULL,
																		session_ids = NULL,
																		item_names = NULL,
																		join = FALSE) {
	
	# 1. Build Query
	query <- list()
	if (!is.null(surveys)) query$surveys <- paste(surveys, collapse = ",")
	if (!is.null(session_ids)) query$sessions <- paste(session_ids, collapse = ",")
	if (!is.null(item_names)) query$items <- paste(item_names, collapse = ",")
	
	# 2. Fetch Data
	res <- formr_api_request(endpoint = paste0("runs/", run_name, "/results"), query = query)
	
	if (length(res) == 0) {
		message("[INFO] No results found.")
		return(dplyr::tibble(session = character()))
	}
	
	# 3. Helper: Clean JSON to Tibble
	clean_json_to_df <- function(rows) {
		if (length(rows) == 0) return(dplyr::tibble(session = character()))
		df <- dplyr::bind_rows(rows)
		suppressMessages(readr::type_convert(df, col_types = readr::cols()))
	}
	
	# 4. Handle Shuffles (Extract and separate)
	shuffle_df <- NULL
	if ("shuffles" %in% names(res)) {
		raw_shuffles <- res$shuffles
		res$shuffles <- NULL 
		
		if (length(raw_shuffles) > 0) {
			s_df <- clean_json_to_df(raw_shuffles)
			
			# Pivot shuffles to wide format (shuffle_1, shuffle_2...)
			if (nrow(s_df) > 0 && "unit_id" %in% names(s_df)) {
				if (!"position" %in% names(s_df)) s_df$position <- NA
				
				shuffle_df <- s_df %>%
					dplyr::select(session = .data$run_session, .data$position, .data$unit_id, .data$group) %>%
					dplyr::mutate(
						position_col = dplyr::case_when(
							!is.na(.data$position) ~ paste0("shuffle_", .data$position),
							TRUE ~ paste0("shuffle_unit_", .data$unit_id)
						)
					) %>%
					dplyr::distinct(.data$session, .data$position_col, .keep_all = TRUE) %>%
					tidyr::pivot_wider(
						id_cols = "session",
						names_from = "position_col", 
						values_from = "group",
						values_fn = list(group = toString)
					)
			}
		}
	}
	
	# 5. Convert Survey Lists to Dataframes
	results_list <- purrr::map(res, clean_json_to_df)
	
	# Add shuffles to the list if they exist
	if (!is.null(shuffle_df)) {
		results_list$shuffles <- shuffle_df
	}
	
	# 6. Return
	if (join) {
		return(.join_results(results_list))
	} else {
		# If only one item and it's not a list of surveys, return the DF directly
		if (length(results_list) == 1 && is.null(shuffle_df)) return(results_list[[1]])
		return(results_list)
	}
}

#' Apply Type Definitions and Labels
#' 
#' @param item_list A data frame containing item metadata.
#' @param results A data frame containing the raw results.
#' @export
formr_api_recognise <- function(item_list, results) {
	# Safety checks
	if (is.null(results) || nrow(results) == 0) return(results)
	if (is.null(item_list) || nrow(item_list) == 0) return(results)
	
	# 1. Timestamp Recognition
	time_cols <- intersect(names(results), c("created", "modified", "ended"))
	for(col in time_cols) {
		if(!inherits(results[[col]], "POSIXct")) results[[col]] <- as.POSIXct(results[[col]])
		attr(results[[col]], "label") <- paste("Timestamp:", tools::toTitleCase(col))
	}
	
	# 2. Item Recognition
	# Filter metadata to items that actually exist in this result set
	items_to_process <- item_list[item_list$name %in% names(results), ]
	
	if(nrow(items_to_process) == 0) {
		return(results)
	}
	
	for (i in seq_len(nrow(items_to_process))) {
		# Use list indexing to handle potential nested columns safely
		item_row <- items_to_process[i, ]
		name <- item_row$name
		type <- item_row$type
		
		# --- FEATURE: APPLY LABELS ---
		# Extract label (prefer 'label', fallback to empty)
		raw_label <- if(!is.null(item_row$label)) item_row$label else ""
		# Clean the label: Remove HTML tags
		clean_label <- trimws(gsub("<[^>]+>", "", raw_label))
		
		# Apply the attribute
		if (length(clean_label) > 0 && !is.na(clean_label)) {
			attr(results[[name]], "label") <- clean_label
		}
		# -----------------------------
		
		# Handle Choices (Factors/Labelled numeric)
		if ("choices" %in% names(item_row) && !is.null(item_row$choices[[1]])) {
			choices <- item_row$choices[[1]]
			
			if (length(choices) > 0) {
				val <- results[[name]]
				
				# Try to determine values and labels
				num_vals <- suppressWarnings(as.numeric(names(choices)))
				
				if(!any(is.na(num_vals))) {
					vals <- num_vals; labs <- unlist(choices)
				} else {
					vals <- unlist(choices); labs <- names(choices)
					if(is.null(labs)) labs <- vals
				}
				
				if(!is.numeric(val)) results[[name]] <- suppressWarnings(as.numeric(val))
				tryCatch({
					results[[name]] <- haven::labelled(results[[name]], stats::setNames(vals, labs))
				}, error = function(e) {
					warning(sprintf("Failed to apply labels to '%s': %s", name, e$message))
				})
			}
		} 
		# Handle Explicit Types
		else if (type %in% c("date", "datetime")) {
			results[[name]] <- tryCatch(as.POSIXct(results[[name]]), error = function(e) results[[name]])
		} else if (type %in% c("number", "range", "calculate")) {
			results[[name]] <- suppressWarnings(as.numeric(results[[name]]))
		}
	}
	results
}

#' Reverse Items and Update Labels
#' 
#' Reverses numeric items ending in 'R' based on metadata bounds.
#' Safely handles cases where choices or labels were accidentally stored as strings.
#'
#' @param results A data frame containing the results.
#' @param item_list A data frame containing item metadata.
#' @export
formr_api_reverse <- function(results, item_list) {
	if (is.null(item_list)) return(results)
	
	# Detect R items (e.g. bfi_10R)
	item_names <- names(results)
	reversed_vars <- item_names[stringr::str_detect(item_names, "(?i)[a-z0-9_]+?[0-9]+R$")]
	
	for (var in reversed_vars) {
		val_vec <- results[[var]]
		
		# 1. Fetch Metadata
		meta <- item_list[item_list$name == var, ]
		if(nrow(meta) == 0) next
		
		choices <- if(is.list(meta$choices)) meta$choices[[1]] else meta$choices
		if(is.null(choices)) next 
		
		# 2. Determine Bounds (Min/Max)
		# Try to parse choices as numbers. 
		# If choices are c("A", "B"), as.numeric produces warnings + NAs.
		meta_vals <- suppressWarnings(as.numeric(unlist(choices)))
		
		# Fallback: Sometimes values are keys, sometimes they are the vector content
		if(all(is.na(meta_vals)) && !is.null(names(choices))) {
			meta_vals <- suppressWarnings(as.numeric(names(choices)))
		}
		
		# 2b. ABORT if metadata is strictly non-numeric (e.g. "Strongly Agree", "Agree")
		if(all(is.na(meta_vals))) {
			warning(sprintf("Skipped reversal for '%s': Choices are non-numeric strings.", var))
			next 
		}
		
		max_val <- max(meta_vals, na.rm = TRUE)
		min_val <- min(meta_vals, na.rm = TRUE)
		reversal_const <- max_val + min_val
		
		# 3. Prepare Data (Safe Conversion)
		# We must strip attributes first to check the raw data vector
		raw_data <- as.vector(val_vec) 
		
		if (is.character(raw_data)) {
			# Try to coerce string numbers ("1", "2") to numeric
			converted_data <- suppressWarnings(as.numeric(raw_data))
			
			# Check if we lost data (e.g. "3" -> 3 is ok, but "High" -> NA is bad)
			was_na_before <- is.na(raw_data)
			is_na_now     <- is.na(converted_data)
			
			if (sum(is_na_now) > sum(was_na_before)) {
				warning(sprintf("Skipped reversal for '%s': Data contains non-numeric strings.", var))
				next
			}
			raw_data <- converted_data
		}
		
		# 4. Perform Numeric Reversal
		# Formula: (Max + Min) - Value
		reversed_numeric <- reversal_const - raw_data
		
		# 5. Handle Attributes (haven_labelled)
		if (inherits(val_vec, "haven_labelled")) {
			old_labels <- attr(val_vec, "labels")
			
			if (!is.null(old_labels)) {
				# 5a. Ensure labels are numeric before math
				old_label_values <- unname(old_labels) # The values, e.g. 1, 2, 3
				
				# If labels are stored as strings "1", "2", convert them
				if (is.character(old_label_values)) {
					old_label_values <- suppressWarnings(as.numeric(old_label_values))
				}
				
				# If labels are non-numeric ("A", "B"), we cannot reverse the label definition.
				# We apply the numeric reversal to the data, but we must DROP the labels 
				# to avoid mismatch, or warn.
				if (any(is.na(old_label_values))) {
					warning(sprintf("Reversed data for '%s', but dropped labels: Label values were non-numeric.", var))
					results[[var]] <- reversed_numeric
				} else {
					# Safe to reverse labels
					new_values <- reversal_const - old_label_values
					
					# Reconstruct valid haven object
					new_labels_vec <- stats::setNames(new_values, names(old_labels))
					
					results[[var]] <- haven::labelled(
						reversed_numeric, 
						labels = sort(new_labels_vec)
					)
				}
			} else {
				results[[var]] <- reversed_numeric
			}
			
			# Keep variable label (the question text)
			if (!is.null(attr(val_vec, "label"))) {
				attr(results[[var]], "label") <- attr(val_vec, "label")
			}
			
		} else {
			# Standard Numeric Case
			results[[var]] <- reversed_numeric
			# Preserve label if exists
			if (!is.null(attr(val_vec, "label"))) {
				attr(results[[var]], "label") <- attr(val_vec, "label")
			}
		}
		
		attr(results[[var]], "reversed") <- TRUE
	}
	
	results
}

#' Aggregate Scales
#' 
#' @param results A data frame/tibble containing the run results.
#' @param item_list A data frame containing item metadata (names, types, choices).
#' @param min_items Minimum number of valid items required to calculate a mean (default 2).
#' @export
formr_api_aggregate <- function(results, item_list, min_items = 2) {
	# Find stems (e.g. "bfi" from "bfi_1", "bfi_2")
	item_names <- names(results)
	stems <- stringr::str_match(item_names, "^([a-zA-Z0-9_]+?)[_]?\\d+[R]?$")[, 2]
	unique_stems <- unique(stats::na.omit(stems))
	unique_stems <- unique_stems[!grepl("^shuffle", unique_stems)] # Ignore shuffles
	
	for (scale in unique_stems) {
		if (scale %in% names(results)) next 
		
		# Find all items belonging to this scale
		pattern <- paste0("^", scale, "[_]?\\d+[R]?$")
		scale_cols <- item_names[stringr::str_detect(item_names, pattern)]
		
		if (length(scale_cols) < min_items) next
		
		# Ensure they are numeric
		is_num <- vapply(results[scale_cols], function(x) is.numeric(x) || inherits(x, "haven_labelled"), logical(1))
		valid_cols <- scale_cols[is_num]
		
		if (length(valid_cols) >= min_items) {
			# Calculate Mean
			subset_df <- as.data.frame(lapply(results[valid_cols], as.numeric))
			results[[scale]] <- rowMeans(subset_df, na.rm = FALSE) 
			
			# Document it
			attr(results[[scale]], "scale_items") <- valid_cols
			attr(results[[scale]], "label") <- paste(scale, "Scale (Mean of", length(valid_cols), "items)")
		}
	}
	results
}

#' Helper: Join results safely
#' @noRd
.join_results <- function(results_list) {
	# FIX: Handle empty list to prevent subscript out of bounds error
	if (length(results_list) == 0) return(dplyr::tibble(session = character()))
	
	if (length(results_list) == 1) return(results_list[[1]])
	
	results_list_renamed <- purrr::imap(results_list, function(df, name) {
		if (name == "shuffles") return(df)
		
		cols_to_rename <- setdiff(names(df), "session")
		if (length(cols_to_rename) > 0) {
			new_names <- paste0(cols_to_rename, "_", name)
			names(df)[match(cols_to_rename, names(df))] <- new_names
		}
		return(df)
	})
	
	joined_df <- purrr::reduce(results_list_renamed, function(x, y) {
		dplyr::full_join(x, y, by = "session")
	})
	
	joined_df
}

#' Helper: Extract relevant items for a specific survey from the Run Structure
#' @noRd
.extract_items_for_survey <- function(structure, survey_name) {
	if (is.null(structure$units)) return(NULL)
	
	for (unit in structure$units) {
		if (unit$type == "Survey" && !is.null(unit$survey_data$name)) {
			
			str_name <- trimws(unit$survey_data$name)
			req_name <- trimws(survey_name)
			
			if (str_name == req_name) {
				items <- unit$survey_data$items
				if (is.null(items) || length(items) == 0) return(NULL)
				return(dplyr::bind_rows(items))
			}
		}
	}
	return(NULL)
}