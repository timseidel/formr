#' Upload File to Run
#' @param run_name Name of the run.
#' @param path Local path to the file.
#' @export
formr_upload_file <- function(run_name, path) {
	if (!file.exists(path)) stop("File not found")
	
	body <- list(file = httr::upload_file(path))
	formr_api_request(paste0("runs/", run_name, "/files"), method = "POST", body = body)
}
