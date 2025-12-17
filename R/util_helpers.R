#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
NULL

#' Get item metadata from survey results
#' 
#' Extracts the item metadata (type, label, choices) attached to the columns 
#' of a results data frame by `formr_recognise`.
#' 
#' @param survey A data frame of results (processed by `formr_recognise`).
#' @return A tibble containing metadata for all items found.
#' @export
items <- function(survey) {
	
	# Extract "item_meta" attribute from each column
	meta_list <- lapply(survey, function(col) attr(col, "item_meta"))
	
	# Filter out NULLs (columns without metadata, like system cols)
	meta_list <- Filter(Negate(is.null), meta_list)
	
	if (length(meta_list) == 0) {
		warning("No item metadata found. Did you run formr_recognise()?")
		return(tibble::tibble())
	}
	
	# Bind into a single tibble
	# Use bind_rows to handle potential missing fields gracefully
	meta_df <- dplyr::bind_rows(meta_list)
	
	return(meta_df)
}

#' Get specific item metadata
#' 
#' Extracts metadata for a single item.
#' 
#' @param survey A data frame of results.
#' @param item_name The name of the column/item.
#' @return A list containing the item's metadata.
#' @export
item <- function(survey, item_name) {
	if (!item_name %in% names(survey)) {
		stop(sprintf("Item '%s' not found in survey.", item_name))
	}
	
	meta <- attr(survey[[item_name]], "item_meta")
	
	if (is.null(meta)) {
		warning(sprintf("No metadata found for item '%s'.", item_name))
		return(NULL)
	}
	
	return(meta)
}

#' Switch choice values with labels
#' 
#' Replaces numeric values with their corresponding text labels based on 
#' `haven::labelled` attributes or item metadata.
#' 
#' @param survey A data frame of results.
#' @param item_name The name of the item.
#' @return A character vector of labels.
#' @export
choice_labels_for_values <- function(survey, item_name) {
	col <- survey[[item_name]]
	
	# 1. Try haven::as_factor (Best for haven::labelled columns)
	if (inherits(col, "haven_labelled")) {
		return(as.character(haven::as_factor(col)))
	}
	
	# 2. Fallback: Manually look up in attributes
	# This handles cases where haven attributes might be stripped but "item_meta" remains
	meta <- attr(col, "item_meta")
	if (!is.null(meta) && !is.null(meta$choices)) {
		choices <- meta$choices[[1]] # Handle list-column structure
		
		# Map values to labels
		# choices is usually: list(Label = Value) or c(Label = Value)
		# We need: Value -> Label mapping
		vals <- unlist(choices)
		labs <- names(choices)
		
		# Create lookup vector
		lookup <- setNames(labs, vals)
		
		# Map
		return(unname(lookup[as.character(col)]))
	}
	
	warning("No labels found for this item.")
	return(as.character(col))
}

#' Random date in range
#' 
#' Helper to generate random timestamps for simulation.
#' 
#' @param N Number of dates to generate.
#' @param lower Start date (YYYY-MM-DD).
#' @param upper End date (YYYY-MM-DD).
#' @export
random_date_in_range <- function(N, lower = "2020-01-01", upper = "2025-12-31") {
	st <- as.POSIXct(as.Date(lower))
	et <- as.POSIXct(as.Date(upper))
	dt <- as.numeric(difftime(et, st, units = "sec"))
	
	# Generate random seconds and add to start time
	st + sort(stats::runif(N, 0, dt))
}

#' Generate email CID
#' 
#' Formats a string as a Content-ID for embedding images in emails.
#' 
#' @param x The image identifier (e.g., "plot_123").
#' @param ext File extension (default ".png").
#' @export
email_image <- function(x, ext = ".png") {
	# Sanitize ID: Remove non-alphanumeric chars
	cid <- gsub("[^a-zA-Z0-9]", "", x)
	
	# Create string with 'link' attribute (used by knitr hooks)
	structure(
		paste0("cid:", cid, ext), 
		link = x
	)
}

#' Get OpenCPU RDS
#' @param session_url The OpenCPU session URL.
#' @param local Logical. Read from local temp dir?
#' @export
get_opencpu_rds = function(session_url, local = TRUE) {
	if (local) {
		# File system read logic
		filepath = stringr::str_match(session_url, "/ocpu/tmp/([xa-f0-9]+)/([a-z0-9A-Z/.]+)")
		sessionfile <- file.path("/tmp/ocpu-www-data/tmp_library", filepath[, 2], ".RData")
		if (file.exists(sessionfile)) {
			sessionenv <- new.env()
			load(sessionfile, envir = sessionenv)
			obj_name = stringr::str_sub(filepath[, 3], 3, -5)
			return(sessionenv[[obj_name]])
		}
	} 
	readRDS(gzcon(curl::curl(session_url)))
}

#' Internal Helper: Extract and Clean Items from Run Structure
#' @noRd
.extract_items_from_run <- function(run_struct) {
	all_items <- list()
	
	# Loop through all units in the run
	for (unit in run_struct$units) {
		# We only care about units of type "Survey" that have data
		if (identical(unit$type, "Survey") && !is.null(unit$survey_data$items)) {
			
			# Process each item in this survey
			survey_items_cleaned <- lapply(unit$survey_data$items, function(x) {
				# Ensure 'choices' and other complex fields are wrapped as lists
				# (This matches the logic in formr_survey_structure)
				complex_fields <- c("input_attributes", "parent_attributes", "allowed_classes", "choices", "val_errors", "val_warnings")
				for (field in complex_fields) {
					if (!is.null(x[[field]])) {
						x[[field]] <- list(x[[field]]) 
					} else {
						x[[field]] <- list(NULL)
					}
				}
				
				# Replace NULLs with NAs for simple fields to allow binding
				x <- lapply(x, function(val) if (is.null(val)) NA else val)
				return(tibble::as_tibble(x))
			})
			
			# Add to our master list
			all_items <- c(all_items, survey_items_cleaned)
		}
	}
	
	if (length(all_items) == 0) {
		warning("No items found in the provided Run Structure.")
		return(tibble::tibble())
	}
	
	# Combine into one big metadata table
	dplyr::bind_rows(all_items)
}