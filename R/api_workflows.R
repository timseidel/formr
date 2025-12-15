# Action: The "Backup" functions are valuable high-level workflows. 
# They currently use legacy code. We should rewrite them to use your new 
# V1 functions and place them here.
# 
# formr_backup_study -> Rewrite to use formr_run_structure, formr_results, etc.
# formr_backup_surveys -> Rewrite to use formr_items, formr_results.
# formr_backup_files -> Rewrite to use formr_uploaded_files.

#' Backup a study
#'
#' Backup a study by downloading all surveys, results, item displays, run shuffle, user overview and user details. This function will save the data in a folder named after the study.
#'
#' @param study_name case-sensitive name of a study your account owns
#' @param save_path path to save the study data, defaults to the study name
#' @param host defaults to [formr_last_host()], which defaults to https://formr.org
#' @param overwrite should existing files be overwritten?
#' @export
#' @examples
#' \dontrun{
#' formr_backup_study(study_name = 'training_diary' )
#' }
formr_backup_study = function(study_name, save_path = study_name, host = formr_last_host(), overwrite = FALSE) {
  run_structure = formr_run_structure(study_name, host)

  if(file.exists(save_path) && !overwrite) {
    stop("Save path already exists. Set overwrite = TRUE to overwrite.")
  }
  # create a folder for the study
  dir.create(save_path, showWarnings = FALSE)
  # save JSON copy of run structure
  jsonlite::write_json(run_structure, 
                        path = paste0(save_path, "/run_structure.json"), 
                        pretty = TRUE)

  # Loop through run structure to find all surveys
  surveys = list()
  for (unit in run_structure$units) {
    if (unit$type == "Survey") {
      surveys[[unit$survey_data$name]] = unit
    }
  }

  survey_names = names(surveys)

  formr_backup_surveys(survey_names, surveys, save_path, overwrite, host)

  # Download run shuffle, if exists
  if ("shuffle" %in% names(run_structure)) {
    shuffle = formr_shuffled(study_name, host)
    jsonlite::write_json(shuffle, 
                         path = paste0(save_path, "/run_shuffle.json"), 
                         pretty = TRUE)
  }

  # Download run user overview
  user_overview = formr_user_overview(study_name, host)
  jsonlite::write_json(user_overview, 
                        path = paste0(save_path, "/run_user_overview.json"), 
                        pretty = TRUE)

  # Download run user details
  user_detail = formr_user_detail(study_name, host)
  jsonlite::write_json(user_detail, 
                        path = paste0(save_path, "/run_user_detail.json"), 
                        pretty = TRUE)

}

#' Download random groups
#'
#' formr has a specific module for randomisation.
#' After connecting using [formr_connect()]
#' you can download the assigned random groups and merge them with your data.
#'
#' @param run_name case-sensitive name of the run in which you randomised participants
#' @param host defaults to [formr_last_host()], which defaults to https://formr.org
#' @export
#' @examples
#' \dontrun{
#' formr_connect(email = 'you@@example.net', password = 'zebrafinch' )
#' formr_shuffled(run_name = 'different_drills' )
#' }

formr_shuffled = function(run_name, host = formr_last_host()) {
	resp = httr::GET(paste0(host, "/admin/run/", run_name, "/random_groups_export?format=json"))
	if (resp$status_code == 200) 
		jsonlite::fromJSON(httr::content(resp, encoding = "utf8", 
																		 as = "text")) else stop("This run does not exist.")
}

#' Download random groups
#'
#' formr collects information about users' progression through the run
#' After connecting using [formr_connect()]
#' you can download a table showing where they are in the run.
#'
#' @param run_name case-sensitive name of the run in which you randomised participants
#' @param host defaults to [formr_last_host()], which defaults to https://formr.org
#' @export
#' @examples
#' \dontrun{
#' formr_connect(email = 'you@@example.net', password = 'zebrafinch' )
#' formr_user_overview(run_name = 'different_drills' )
#' }

formr_user_overview = function(run_name, host = formr_last_host()) {
	resp = httr::GET(paste0(host, "/admin/run/", run_name, "/export_user_overview?format=json"))
	if (resp$status_code == 200) 
		jsonlite::fromJSON(httr::content(resp, encoding = "utf8", 
																		 as = "text")) else stop("This run does not exist.")
}


#' Download random groups
#'
#' formr collects information about users' progression through the run
#' After connecting using [formr_connect()]
#' you can download a table showing their progression through the run.
#'
#' @param run_name case-sensitive name of the run in which you randomised participants
#' @param host defaults to [formr_last_host()], which defaults to https://formr.org
#' @export
#' @examples
#' \dontrun{
#' formr_connect(email = 'you@@example.net', password = 'zebrafinch' )
#' formr_user_detail(run_name = 'different_drills' )
#' }

formr_user_detail = function(run_name, host = formr_last_host()) {
	resp = httr::GET(paste0(host, "/admin/run/", run_name, "/export_user_detail?format=json"))
	if (resp$status_code == 200) 
		jsonlite::fromJSON(httr::content(resp, encoding = "utf8", 
																		 as = "text")) else stop("This run does not exist.")
}


#' Backup uploaded files from formr
#'
#' After connecting to formr using [formr_connect()]
#' you can backup uploaded files using this command.
#'
#' @param survey_name case-sensitive name of a survey your account owns
#' @param overwrite should existing files be overwritten? defaults to FALSE
#' @param save_path defaults to the survey name
#' @param host defaults to [formr_last_host()], which defaults to https://formr.org
#' @export
#' @examples
#' \dontrun{
#' formr_backup_files(survey_name = 'training_diary' )
#' }

formr_backup_files = function(survey_name, 
															overwrite = FALSE, 
															save_path = paste0(survey_name, "/user_uploaded_files"),
															host = formr_last_host()) {
	file_list = formr_uploaded_files(survey_name, host)
	if(length(file_list) > 0) {
		dir.create(save_path, showWarnings = FALSE)
		message("Downloading ", length(file_list), " user-uploaded files...")
		i = 0
		for (file in file_list) {
			i = i + 1
			local_file_name = basename(file$stored_path)
			local_file_name = paste0(save_path, "/", local_file_name)
			resp = httr::GET(file$stored_path)
			if (resp$status_code != 200) {
				warning("Could not download file ", local_file_name)
				file_list[[i]]$downloaded <- FALSE
			} else {
				if(overwrite | !file.exists(local_file_name)) {
					raw_content <- httr::content(resp, as = "raw")
					writeBin(raw_content, local_file_name)
				}
				file_list[[i]]$downloaded <- TRUE
			}
		}
	}
	invisible(file_list)
}
