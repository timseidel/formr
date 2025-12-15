#' List Sessions in a Run
#' @param run_name Name of the run.
#' @param active Filter: TRUE for ongoing, FALSE for finished, NULL for all.
#' @param limit Pagination limit.
#' @export
formr_sessions <- function(run_name, active = NULL, limit = 100) {
	query <- list(limit = limit)
	if (!is.null(active)) query$active <- if(active) 1 else 0
	
	formr_api_request(paste0("runs/", run_name, "/sessions"), query = query)
}

#' Create Session(s)
#' @param run_name Name of the run.
#' @param codes Character vector of codes. If NULL, creates one random code.
#' @param testing Logical. Mark these sessions as testing?
#' @export
formr_create_session <- function(run_name, codes = NULL, testing = FALSE) {
	body <- list(testing = if(testing) 1 else 0)
	if (!is.null(codes)) body$code <- codes
	
	formr_api_request(paste0("runs/", run_name, "/sessions"), method = "POST", body = body)
}

#' Perform Action on Session
#' @param run_name Name of the run.
#' @param session_code The session ID/code.
#' @param action One of: "end_external", "toggle_testing", "move_to_position".
#' @param position Required if action is "move_to_position".
#' @export
formr_session_action <- function(run_name, session_code, action, position = NULL) {
	body <- list(action = action)
	if (!is.null(position)) body$position <- position
	
	formr_api_request(paste0("runs/", run_name, "/sessions/", session_code, "/actions"), method = "POST", body = body)
}