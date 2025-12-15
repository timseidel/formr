#' Transform formr_item_list into a data.frame
#' @export
as.data.frame.formr_item_list = function(x, row.names, ...) {
	item_list = x
	names(item_list) = NULL
	for (i in seq_along(item_list)) {
		# Replace NULLs with NA
		item_list[[i]][sapply(item_list[[i]], is.null)] <- NA
		
		# Collapse choices list to string for DF display
		if (!is.null(item_list[[i]]$choices)) {
			item_list[[i]]$choices = paste(paste0(names(item_list[[i]]$choices), "=", item_list[[i]]$choices), collapse = ",")
		}
		# Convert lists to chars
		for(f in c("type_options", "choice_list", "value", "showif", "class")) {
			item_list[[i]][[f]] = as.character(item_list[[i]][[f]])
		}
	}
	df <- data.frame(dplyr::bind_rows(item_list))
	df$index = 1:nrow(df)
	df
}

#' Get item list from survey attributes
#' @export
items = function(survey) {
	vars = names(survey)
	item_list = list()
	for (var in vars) {
		att = attributes(survey[[var]])
		if (!is.null(att$item) && !exists("scale", att)) {
			item_list[[var]] = att$item
		}
	}
	class(item_list) = c("formr_item_list", class(item_list))
	item_list
}

#' Get specific item from survey
#' @export
item = function(survey, item_name) {
	att = attributes(survey[[item_name]])
	if (!is.null(att$item)) return(att$item)
	warning("No item info found.")
	NULL
}

#' Switch choice values with labels
#' @export
choice_labels_for_values = function(survey, item_name) {
	choices = item(survey, item_name)$choices
	unname(unlist(choices)[survey[[item_name]]])
}

#' Random date in range
#' @export
random_date_in_range <- function(N, lower = "2012/01/01", upper = "2012/12/31") {
	st <- as.POSIXct(as.Date(lower))
	et <- as.POSIXct(as.Date(upper))
	dt <- as.numeric(difftime(et, st, units = "sec"))
	st + sort(stats::runif(N, 0, dt))
}

#' Generate email CID
#' @export
email_image = function(x, ext = ".png") {
	cid = gsub("[^a-zA-Z0-9]", "", substring(x, 8))
	structure(paste0("cid:", cid, ext), link = x)
}

#' Get OpenCPU RDS
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