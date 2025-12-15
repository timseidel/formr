#' @importFrom dplyr "%>%"
#' @importFrom stats setNames
#' @export
dplyr::`%>%`

#' Reverse labelled values
#' 
#' Reverses the underlying values for a numeric [haven::labelled()] vector 
#' while preserving and updating the value labels correctly.
#' Useful for reversing Likert scales (e.g., 1=Strongly Disagree -> 5=Strongly Agree).
#'
#' @param x A numeric vector, potentially with `haven::labelled` attributes.
#' @return A vector of the same class with values reversed.
#' @export
#' @examples
#' x <- haven::labelled(rep(1:3, each = 3), c(Bad = 1, Good = 5))
#' x
#' reverse_labelled_values(x)
reverse_labelled_values <- function(x) {
	
	# 1. Handle Factors (Convert to numeric with warning)
	if (is.factor(x)) {
		warning(sprintf("Converting factor '%s' to numeric for reversal.", deparse(substitute(x))))
		# Save levels as labels before converting
		lbls <- setNames(seq_along(levels(x)), levels(x))
		x <- as.numeric(x)
		attr(x, "labels") <- lbls
	}
	
	# 2. Extract Metadata
	labels <- attr(x, "labels")
	if (is.null(labels)) {
		# If no labels, just reverse based on range in data
		vals <- stats::na.omit(unique(x))
		if (length(vals) == 0) return(x)
		min_val <- min(vals)
		max_val <- max(vals)
		return((max_val + min_val) - x)
	}
	
	# 3. Determine Range
	val_range <- unname(labels)
	
	# Safety Check: Ensure data fits within defined labels
	# If data contains 6 but scale is only defined 1-5, reversal is ambiguous.
	data_vals <- stats::na.omit(unique(as.numeric(x)))
	scale_min <- min(val_range, na.rm = TRUE)
	scale_max <- max(val_range, na.rm = TRUE)
	
	if (any(data_vals < scale_min | data_vals > scale_max)) {
		warning(sprintf("Values outside labelled range [%s, %s] detected. Reversal may be incorrect.", 
										scale_min, scale_max))
		# Expand range to include data extremes to avoid corruption
		scale_min <- min(c(scale_min, data_vals))
		scale_max <- max(c(scale_max, data_vals))
	}
	
	# 4. Perform Reversal
	# Formula: New = (Max + Min) - Old
	# This works for any linear scale (0-based, 1-based, negative, etc.)
	reversal_constant <- scale_max + scale_min
	new_x <- reversal_constant - as.numeric(x)
	
	# 5. Update Labels
	# We must also reverse the values associated with the labels
	new_labels <- setNames(reversal_constant - val_range, names(labels))
	
	# Restore attributes
	attributes(new_x) <- attributes(x)
	attr(new_x, "labels") <- sort(new_labels) # Sort for tidiness
	
	new_x
}

#' Helper: Safe Type Conversion
#' @noRd
as_same_type_as <- function(target, obj) {
	if (is.numeric(target)) {
		suppressWarnings(as.numeric(obj))
	} else {
		methods::as(obj, class(target)[1])
	}
}

#' Aggregate variables and document construction
#'
#' Computes a row-wise aggregate (mean, sum) and attaches metadata about 
#' which items were included. Useful for transparency in codebooks.
#'
#' @param items A data.frame/tibble of the items to aggregate.
#' @param fun Aggregation function (default: `rowMeans`).
#' @param stem Optional manual stem name. If NULL, auto-detects longest common prefix.
#' @return A numeric vector with attributes `scale_item_names` and `label`.
#' @export
#' @examples
#' df <- data.frame(e1 = 1:5, e2 = 5:1, e3 = 1:5)
#' scale_score <- aggregate_and_document_scale(df)
#' attr(scale_score, "label")
aggregate_and_document_scale <- function(items, fun = rowMeans, stem = NULL) {
	
	# Calculate Score
	# We convert to matrix to ensure rowMeans works on tibbles safely
	new_scale <- fun(as.matrix(items), na.rm = TRUE)
	
	item_names <- names(items)
	
	# Auto-detect common stem (e.g. "bfi_n_1", "bfi_n_2" -> "bfi_n")
	if (is.null(stem)) {
		stem <- ""
		if (length(item_names) > 1) {
			# Simple heuristic: compare first and last item names
			# (Robust enough for typical item naming conventions)
			common <- tryCatch({
				# Find longest common substring of first two items
				# Then trim trailing numbers/underscores
				s1 <- item_names[1]
				s2 <- item_names[2]
				# Basic character matching loop (vectorized substring is messy)
				for(i in nchar(s1):1) {
					sub <- substr(s1, 1, i)
					if (startsWith(s2, sub)) {
						stem <- sub
						break
					}
				}
				stem
			}, error = function(e) "")
		} else {
			stem <- item_names[1]
		}
		
		# Clean up stem: remove trailing numbers, underscores, or 'R'
		# e.g. "bfi_n_" -> "bfi_n"
		stem <- sub("[_0-9R]+$", "", stem)
	}
	
	# Attach Metadata
	attr(new_scale, "scale_item_names") <- item_names
	attr(new_scale, "label") <- sprintf("%d %s items aggregated by %s", 
																			ncol(items), stem, deparse(substitute(fun)))
	
	new_scale
}

#' Rescue lost attributes
#'
#' Copies attributes (labels, SPSS formats) from one data frame to another.
#' Useful after `dplyr` operations that strip attributes.
#'
#' @param df_target The data frame missing attributes.
#' @param df_source The reference data frame containing attributes.
#' @return `df_target` with restored attributes.
#' @export
rescue_attributes <- function(df_target, df_source) {
	
	# Identify common columns
	common_cols <- intersect(names(df_target), names(df_source))
	
	for (col in common_cols) {
		# If target has NO attributes, copy all from source
		if (is.null(attributes(df_target[[col]]))) {
			attributes(df_target[[col]]) <- attributes(df_source[[col]])
		} else {
			# If target has SOME attributes, merge missing ones
			source_attrs <- attributes(df_source[[col]])
			target_attrs <- attributes(df_target[[col]])
			
			# Find attributes in source that are missing in target
			missing_keys <- setdiff(names(source_attrs), names(target_attrs))
			
			for (key in missing_keys) {
				attr(df_target[[col]], key) <- source_attrs[[key]]
			}
		}
	}
	
	df_target
}