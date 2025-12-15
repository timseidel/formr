#' List all surveys
#' @export
formr_surveys <- function(name = NULL) {
	query <- if(!is.null(name)) list(name = name) else NULL
	formr_api_request("surveys", query = query)
}

#' Download items from formr
#'
#' After connecting to formr using [formr_connect()]
#' you can download items using this command. One of survey_name or path has to be specified, if both are specified, survey_name is preferred.
#'
#' @param survey_name case-sensitive name of a survey your account owns
#' @param host defaults to [formr_last_host()], which defaults to https://formr.org
#' @param path path to local JSON copy of the item table
#' @export
#' @examples
#' \dontrun{
#' formr_connect(email = 'you@@example.net', password = 'zebrafinch' )
#' formr_items(survey_name = 'training_diary' )
#' }
#' formr_items(path = 
#' 	system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))[1:2]

formr_items = function(survey_name = NULL, host = formr_last_host(), 
											 path = NULL) {
	item_list = NULL
	if (!is.null(survey_name)) {
		resp = httr::GET(paste0(host, "/admin/survey/", survey_name, 
														"/export_item_table?format=json"))
		if (resp$status_code == 200) {
			item_list = jsonlite::fromJSON(txt = httr::content(resp, 
																												 encoding = "utf8", as = "text"), simplifyDataFrame = FALSE)
		} else {
			stop("This survey does not exist.")
		}
	} else {
		item_list = jsonlite::fromJSON(txt = path, simplifyDataFrame = FALSE)
	}
	if (!is.null(item_list)) {
		if (!is.null(item_list[["items"]])) {
			item_list = item_list[["items"]]
		}
		for (i in seq_along(item_list)) {
			if (item_list[[i]]$type == "rating_button") {
				from = 1
				to = 5
				by = 1
				if (!is.null(item_list[[i]]$type_options)) {
					# has the format 1,6 or 1,6,1 + possibly name of choice list
					# allow for 1, 6, 1 and 1,6,1
					item_list[[i]]$type_options <- 
						stringr::str_replace_all(item_list[[i]]$type_options,
																		 ",\\s+", ",")
					# truncate choice list
					sequence = stringr::str_split(item_list[[i]]$type_options, 
																				"\\s", n = 2)[[1]][1]
					sequence = stringr::str_split(sequence, ",")[[1]]
					if (length(sequence) == 3) {
						from = as.numeric(sequence[1])
						to = as.numeric(sequence[2])
						by = as.numeric(sequence[3])
					} else if (length(sequence) == 2) {
						from = as.numeric(sequence[1])
						to = as.numeric(sequence[2])
					} else if (length(sequence) == 1) {
						to = as.numeric(sequence[1])
					}
				}
				sequence = seq(from, to, ifelse(to >= from, by, 
																				ifelse( by > 0, -1 * by, by)))
				names(sequence) = sequence
				if (length(item_list[[i]]$choices) <= 2) {
					choices = item_list[[i]]$choices
					from_pos <- which(sequence == from)
					to_pos <- which(sequence == to)
					sequence[ from_pos ] = paste0(sequence[ from_pos ], ": ", choices[[1]])
					sequence[ to_pos ] = paste0(sequence[ to_pos ], ": ", choices[[length(choices)]])
				} else {
					for (c in seq_along(item_list[[i]]$choices)) {
						sequence[ names(item_list[[i]]$choices)[c] == sequence ]    = paste0(names(item_list[[i]]$choices)[c], ": ", item_list[[i]]$choices[[c]])
					}
				}
				item_list[[i]]$choices = as.list(sequence)
			}
			# named array fails, if names go from 0 to len-1
			if (!is.null(item_list[[i]]$choices) && is.null(names(item_list[[i]]$choices))) {
				names(item_list[[i]]$choices) = 0:(length(item_list[[i]]$choices)-1)
			}
		}
		names(item_list) = sapply(item_list, function(item) { item$name })
		class(item_list) = c("formr_item_list", class(item_list))
		item_list
	} else {
		stop("Have to specify either path to exported JSON file or get item table from formr.")
	}
}

#' Upload/Update Survey
#' 
#' Uploads an item table (Excel, JSON, CSV) to create or update a survey.
#' @param survey_name Name of the survey.
#' @param file_path Path to the file.
#' @export
formr_upload_survey <- function(survey_name, file_path) {
	if (!file.exists(file_path)) stop("File not found: ", file_path)
	
	res <- formr_api_request(
		endpoint = paste0("surveys/", survey_name),
		method = "POST",
		body = list(file = httr::upload_file(file_path)),
		encode = "multipart"
	)
	
	message(sprintf("✅ Survey '%s' uploaded successfully.", res$name))
	invisible(res)
}

#' Download detailed result timings and display counts from formr
#'
#' After connecting to formr using [formr_connect()]
#' you can download detailed times and display counts for each item using this command.
#'
#' @param survey_name case-sensitive name of a survey your account owns
#' @param host defaults to [formr_last_host()], which defaults to https://formr.org
#' @export
#' @examples
#' \dontrun{
#' formr_connect(email = 'you@@example.net', password = 'zebrafinch' )
#' formr_item_displays(survey_name = 'training_diary' )
#' }

formr_item_displays = function(survey_name, host = formr_last_host()) {
  resp = httr::GET(paste0(host, "/admin/survey/", survey_name, 
    "/export_itemdisplay?format=json"))

  if (resp$status_code == 200) {
  	results = jsonlite::fromJSON(httr::content(resp, encoding = "utf8", 
  		as = "text")) 
  } else	{
  		warning("This item display table for this survey could not be accessed.")
	}
  		  
  results
}