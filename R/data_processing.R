#' @importFrom dplyr "%>%" select mutate arrange filter
#' @importFrom tibble as_tibble
#' @importFrom stringr str_detect str_match
#' @importFrom haven labelled tagged_na
#' @importFrom rlang .data
NULL

#' Recognise data types based on survey metadata
#' 
#' Applies type conversion (factors, dates, numbers) and attributes (labels) 
#' to the results table based on the survey structure.
#' 
#' @param item_list Metadata tibble from `formr_survey_structure()`.
#' @param results Results tibble from `formr_results()`.
#' @return The processed results tibble with correct types and attributes.
#' @export
formr_recognise <- function(item_list, results) {
	if (nrow(results) == 0) return(results)
	
	# Timestamp conversion
	time_cols <- intersect(names(results), c("created", "modified", "ended"))
	for(col in time_cols) {
		if(!inherits(results[[col]], "POSIXct")) {
			results[[col]] <- as.POSIXct(results[[col]])
		}
		attr(results[[col]], "label") <- paste("timestamp:", col)
	}
	
	if (is.null(item_list) || nrow(item_list) == 0) return(results)
	
	# Filter metadata to items actually in the results
	items_to_process <- item_list[item_list$name %in% names(results), ]
	
	# Loop safety: use row index
	for (i in seq_len(nrow(items_to_process))) {
		# Extract row as list to safely handle list-columns like 'choices'
		item <- as.list(items_to_process[i, ])
		
		name <- item$name
		type <- item$type
		label <- if(!is.null(item$label)) item$label else ""
		
		# Robust choice extraction (handles list-column nesting from tibble)
		choices <- NULL
		if (!is.null(item$choices)) {
			if (is.list(item$choices)) choices <- item$choices[[1]] # It's nested in the cell
			else choices <- item$choices
		}
		
		# A. Choice Items
		if (!is.null(choices) && length(choices) > 0) {
			val <- results[[name]]
			
			choice_labels <- names(choices)
			choice_values <- unlist(choices)
			
			if (is.null(choice_labels)) {
				choice_labels <- choice_values
				choice_values <- seq_along(choice_values)
			}
			
			# Convert to numeric if possible
			num_values <- suppressWarnings(as.numeric(choice_values))
			if (!any(is.na(num_values))) {
				choice_values <- num_values
				results[[name]] <- suppressWarnings(as.numeric(val))
			}
			
			# Create labelled vector
			labels_vec <- stats::setNames(choice_values, choice_labels)
			try({
				results[[name]] <- haven::labelled(results[[name]], labels_vec)
			}, silent = TRUE)
		}
		# B. Explicit Types
		else if (type %in% c("text", "textarea", "email")) {
			results[[name]] <- as.character(results[[name]])
		} else if (type %in% c("date", "datetime")) {
			# Robust date parsing
			results[[name]] <- tryCatch(as.POSIXct(results[[name]]), error = function(e) results[[name]])
		} else if (type %in% c("number", "range", "calculate")) {
			results[[name]] <- suppressWarnings(as.numeric(results[[name]]))
		}
		
		attr(results[[name]], "label") <- label
		attr(results[[name]], "item_meta") <- item
	}
	
	results
}

#' Process Results Wrapper
#' 
#' The master pipeline for cleaning data.
#' Steps:
#' 1. Filter test sessions (optional).
#' 2. Apply types/attributes (`formr_recognise`).
#' 3. Reverse scored items (`formr_reverse`).
#' 4. Calculate scale means (`formr_aggregate`).
#' 5. Label missing values (optional).
#' 
#' @param item_list Metadata tibble.
#' @param results Results tibble.
#' @param item_displays Optional display log tibble.
#' @param remove_test_sessions Filter out sessions with "XXX".
#' @param tag_missings Tag NAs if display data is present.
#' @param fallback_max Max value for Likert reversal (default 5).
#' @export
formr_post_process_results <- function(item_list, results, 
																			 item_displays = NULL,
																			 remove_test_sessions = TRUE,
																			 tag_missings = !is.null(item_displays),
																			 fallback_max = 5) {
	
	# 1. Filter Test Sessions
	if (remove_test_sessions && "session" %in% names(results)) {
		results <- results[!grepl("XXX", results$session), ]
		
		if (!is.null(item_displays) && "session" %in% names(item_displays)) {
			item_displays <- item_displays[!grepl("XXX", item_displays$session), ]
		}
	}
	
	# 2. Type Recognition
	results <- formr_recognise(item_list, results)
	
	# 3. Reverse Items
	results <- formr_reverse(results, item_list, fallback_max)
	
	# 4. Aggregate Scales
	results <- formr_aggregate(results, item_list)
	
	# 5. Label Missings
	if (tag_missings && !is.null(item_displays)) {
		results <- formr_label_missings(results, item_displays)
	}
	
	results
}

#' Reverse Items
#' 
#' Reverses items ending in 'R' (e.g. `extra_1R`). 
#' Uses metadata to find max value, or falls back to `fallback_max`.
#' 
#' @param results Results tibble.
#' @param item_list Metadata tibble.
#' @param fallback_max Default max for reversal if unknown (default 5).
#' @export
formr_reverse <- function(results, item_list = NULL, fallback_max = 5) {
	
	item_names <- names(results)
	# Detect items ending in number + R (e.g. "bfi_10R", "scale2R")
	reversed_vars <- item_names[stringr::str_detect(item_names, "(?i)[a-z0-9_]+?[0-9]+R$")]
	
	for (var in reversed_vars) {
		# Skip if not numeric/labelled
		if(!is.numeric(results[[var]]) && !inherits(results[[var]], "haven_labelled")) next
		
		# 1. Determine Max
		item_max <- fallback_max
		
		# Try to get max from metadata choices
		if(!is.null(item_list)) {
			meta <- item_list[item_list$name == var, ]
			if(nrow(meta) > 0 && !is.null(meta$choices[[1]])) {
				# Check explicit choices
				choices <- unlist(meta$choices[[1]])
				vals <- suppressWarnings(as.numeric(choices))
				if(!all(is.na(vals))) item_max <- max(vals, na.rm = TRUE)
			}
		}
		
		# Heuristic update: if data > max, bump max
		current_vals <- as.numeric(results[[var]])
		current_max <- max(current_vals, na.rm = TRUE)
		if (is.finite(current_max) && current_max > item_max) {
			item_max <- current_max
		}
		
		# 2. Perform Reversal: (Max + Min) - Value
		# Assumes Min=1. 
		# Formula: new = (Max + 1) - old
		results[[var]] <- (item_max + 1) - as.numeric(results[[var]])
		
		attr(results[[var]], "reversed") <- TRUE
		attr(results[[var]], "reversal_max") <- item_max
	}
	
	results
}

#' Aggregate Scales
#' 
#' Calculates row means for items sharing a common stem.
#' E.g. `bfi_n_1`, `bfi_n_2` -> `bfi_n`.
#' 
#' @param results Results tibble.
#' @param item_list Metadata tibble (optional, for checking types).
#' @param min_items Minimum items required to form a scale (default 2).
#' @export
formr_aggregate <- function(results, item_list = NULL, min_items = 2) {
	
	item_names <- names(results)
	# Regex for stems (e.g. bfi_1 -> bfi)
	stems <- stringr::str_match(item_names, "^([a-zA-Z0-9_]+?)[_]?\\d+[R]?$")[, 2]
	unique_stems <- unique(stats::na.omit(stems))
	
	for (scale in unique_stems) {
		if (scale %in% names(results)) next 
		
		pattern <- paste0("^", scale, "[_]?\\d+[R]?$")
		scale_cols <- item_names[stringr::str_detect(item_names, pattern)]
		
		if (length(scale_cols) < min_items) next
		
		# CRITICAL FIX: Ensure we only aggregate NUMERIC data
		# If it's a Factor, stop. Factors convert to 1,2,3 integers which ruins Likert scales (0,1,2,3)
		safe_cols <- vapply(results[scale_cols], function(x) {
			is.numeric(x) || inherits(x, "haven_labelled")
		}, logical(1))
		
		if (all(safe_cols)) {
			message(sprintf("ℹ Aggregating '%s' (%d items)", scale, length(scale_cols)))
			
			# Strip haven labels for math
			mat <- as.matrix(sapply(results[scale_cols], as.numeric))
			results[[scale]] <- rowMeans(mat, na.rm = TRUE)
			
			attr(results[[scale]], "scale_items") <- scale_cols
			attr(results[[scale]], "label") <- paste(length(scale_cols), "items aggregated (mean)")
		} else {
			# Warn if we skipped because of types
			warning(sprintf("Skipped aggregating '%s': Some items are not numeric/labelled.", scale))
		}
	}
	results
}

#' Tag Missing Values
#' !!Currently disabled!!
#' Uses `item_displays` log to distinguish between:
#' - **Shown but skipped** (User saw it, didn't answer)
#' - **Hidden** (Logic skipped it)
#' - **Not Reached** (User quit before this page)
#' 
#' @param results Results tibble.
#' @param item_displays Display log tibble.
#' @export
formr_label_missings <- function(results, item_displays) {
	# Requires `haven` for tagged NAs
	if (is.null(item_displays) || nrow(item_displays) == 0) return(results)
	
	# TODO: Re-implement robust mapping when `item_displays` structure is confirmed via API V1.
	# The legacy logic relied on specific column names ('shown', 'hidden') that might change.
	
	results
}

#' Simulate Data from Metadata
#' 
#' Generates dummy data based on item types and choices.
#' 
#' @param item_list Metadata tibble.
#' @param n Number of participants to simulate.
#' @export
formr_simulate_from_items <- function(item_list, n = 100) {
	
	# Create skeleton
	df <- tibble::tibble(session = paste0("dummy_", 1:n))
	
	for (i in seq_len(nrow(item_list))) {
		item <- item_list[i, ]
		name <- item$name
		type <- item$type
		choices <- if(!is.null(item$choices)) item$choices[[1]] else NULL
		
		val <- rep(NA, n)
		
		# Simulation Logic
		if (!is.null(choices)) {
			# Sample from choice keys
			keys <- names(choices)
			if(is.null(keys)) keys <- seq_along(choices) # Implicit keys
			
			# Prefer numeric if keys look numeric
			num_keys <- suppressWarnings(as.numeric(keys))
			if(!any(is.na(num_keys))) keys <- num_keys
			
			val <- sample(keys, n, replace = TRUE)
			
		} else if (type == "number") {
			val <- round(runif(n, 0, 100), 1)
		} else if (type == "range") {
			val <- sample(1:100, n, replace = TRUE)
		} else if (type %in% c("text", "textarea")) {
			val <- paste("Text for", name, 1:n)
		} else if (type == "email") {
			val <- paste0("user", 1:n, "@test.com")
		} else if (type %in% c("date", "datetime")) {
			val <- Sys.time() - runif(n, 0, 86400 * 30)
		}
		
		if (type %in% c("note", "submit", "calculate")) next
		
		df[[name]] <- val
	}
	
	# Fake metadata
	df$created <- Sys.time() - runif(n, 0, 10000)
	df$ended <- df$created + runif(n, 60, 600)
	
	df
}