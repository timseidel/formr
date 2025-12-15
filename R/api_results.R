#' Get Run Results
#' 
#' Fetches results for a specific run. Can filter by specific surveys.
#' This replaces the old 'formr_raw_results' which was survey-centric.
#' 
#' @param run_name Name of the run.
#' @param surveys Optional vector of survey names to filter by.
#' @export
formr_results <- function(run_name, surveys = NULL) {
	query <- list()
	if (!is.null(surveys)) query$surveys <- paste(surveys, collapse = ",")
	
	res <- formr_api_request(paste0("runs/", run_name, "/results"), query = query)
	
	# API returns list(survey1 = [...], survey2 = [...])
	# If users asked for a single survey, simplify the output
	if (!is.null(surveys) && length(surveys) == 1 && !is.null(res[[surveys]])) {
		return(res[[surveys]])
	}
	return(res)
}