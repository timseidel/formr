library(testthat)
library(vcr)
library(formr)

test_that("formr_api_authenticate works (live/recorded)", {
	
	vcr::use_cassette("formr_api_authenticate", {
		
		# 1. Authenticate using env vars (loaded automatically from .Renviron)
		formr_api_authenticate(
			host = Sys.getenv("FORMR_HOST"), 
			client_id = Sys.getenv("FORMR_CLIENT_ID"),
			client_secret = Sys.getenv("FORMR_CLIENT_SECRET")
		)
		
		# 2. Verification
		session <- formr_api_session()
		
		# Check that we got a valid token
		expect_type(session$token, "character")
		expect_true(nchar(session$token) > 10)
		
		# Check the host matches
		expected_hostname <- httr::parse_url(Sys.getenv("FORMR_HOST"))$hostname
		expect_equal(session$base_url$hostname, expected_hostname)
	})
})

test_that("formr_runs returns a valid tibble", {
	vcr::use_cassette("formr_runs_list", {
		runs <- formr_runs()
		
		# Check basics
		expect_s3_class(runs, "data.frame")
		expect_true("id" %in% names(runs))
		expect_true("public" %in% names(runs))
		
		# Check type conversion (Critical: ensures api_runs.R logic works)
		# Source: api_runs.R uses as.logical for 'public'
		expect_type(runs$public, "logical")
	})
})

test_that("formr_create_session creates a session and returns code", {
	vcr::use_cassette("formr_create_session_basic", {
		
		# 1. Create session (Action)
		res <- formr_create_session(run_name = "test-run", testing = TRUE)
		
		# 2. Verify response structure
		expect_type(res$sessions, "list") 
		
		# Check that the list contains exactly 1 element
		expect_length(res$sessions, 1)
		
		# Check that the actual code inside is a character string
		expect_type(res$sessions[[1]], "character")
		
	}, match_requests_on = c("method", "uri", "body")) 
})

test_that("formr_session_action moves user position", {
	vcr::use_cassette("formr_session_action_move", {
		
		# 1. Setup: Create a fresh session
		session_res <- formr_create_session("test-run", testing = TRUE)
		code <- session_res$sessions[[1]]
		
		# 2. Action: Move user to position 10 (or any valid position)
		# Source: api_sessions.R formr_session_action
		success <- formr_session_action(
			run_name = "test-run", 
			session_codes = code, 
			action = "move_to_position", 
			position = 10
		)
		
		expect_true(success)
		
		# 3. Verify: Fetch details to prove they actually moved
		details <- formr_session_details("test-run", code)
		expect_equal(as.numeric(details$position), 10)
		
	}, match_requests_on = c("method", "uri", "body"))
})

test_that("formr_results parses types correctly", {
	vcr::use_cassette("formr_results_fetch", {
		
		results <- formr_results(run_name = "test-run")
		
		# Check that basic request worked
		expect_s3_class(results, "tbl_df")
		
		# Check that type conversion happened.
		# api_results.R uses readr::type_convert
		# If this fails, 'created' might be a character string instead of POSIXct
		if ("created" %in% names(results)) {
			expect_s3_class(results$created, "POSIXct")
		}
		
		# Check that 'session' column exists (it's the join key)
		expect_true("session" %in% names(results))
	})
})

test_that("formr_survey_structure parses nested choices correctly", {
	vcr::use_cassette("formr_survey_structure_items", {
		
		# Replace 'daily_survey' with a real survey name from your test run
		items <- formr_survey_structure("platzhalter")
		
		# 1. Check it returns a tibble
		expect_s3_class(items, "tbl_df")
		
		# 2. Check essential columns exist
		expect_true(all(c("name", "type", "label") %in% names(items)))
		
		# 3. CRITICAL: Check that 'choices' is parsed as a list-column
		# The API returns JSON arrays, which should become a list in R, not a character string
		if ("choices" %in% names(items)) {
			expect_type(items$choices, "list")
		}
	})
})

test_that("formr_upload_file works (multipart request)", {
	vcr::use_cassette("formr_upload_delete_flow", {
		
		# FIX: Use .txt because writeLines creates a text file
		tmp_file <- "test_upload.txt" 
		
		writeLines("This is a test file", tmp_file)
		on.exit(unlink(tmp_file)) 
		
		# 2. Upload
		res <- formr_upload_file(run_name = "test-run", path = tmp_file)
		
		# 3. Verification
		files <- formr_files("test-run")
		expect_true(tmp_file %in% files$name)
		
		# 4. Cleanup
		formr_delete_file("test-run", tmp_file)
		
		# Verify deletion
		files_after <- formr_files("test-run")
		expect_false(tmp_file %in% files_after$name)
	})
})

test_that("formr_survey_structure returns valid item metadata", {
	vcr::use_cassette("formr_survey_structure_fetch", {
		
		# 1. Get list of all surveys (to find a valid name dynamically)
		surveys <- formr_surveys()
		
		# Skip if account has no surveys
		if (nrow(surveys) == 0) skip("No surveys found on this account to test.")
		
		# 2. Fetch structure for the first survey found
		target_survey <- surveys$name[1]
		items <- formr_survey_structure(target_survey)
		
		# 3. Verify Tibble Structure
		expect_s3_class(items, "tbl_df")
		expect_true(all(c("name", "type", "label") %in% names(items)))
		
		# 4. Critical: Verify List-Columns (Choices)
		# The API returns choices as nested JSON. R should see this as a 'list'.
		# If this fails, readr/jsonlite parsing logic might have changed.
		if ("choices" %in% names(items)) {
			expect_type(items$choices, "list")
		}
	})
})

test_that("formr_run_structure can import (PUT) a valid JSON", {
	vcr::use_cassette("formr_run_structure_import", {
		
		# 1. Setup: Create a minimal valid Run Structure JSON
		# This represents a run with 1 survey unit
		minimal_structure <- '{
      "name": "test-run",
      "units": [
        {
          "type": "Pause",
          "position": 1,
          "wait_minutes": 10
        }
      ]
    }'
		
		tmp_json <- "test_structure.json"
		writeLines(minimal_structure, tmp_json)
		on.exit(unlink(tmp_json))
		
		# 2. Action: Import the structure (PUT request)
		# Source: api_runs.R formr_run_structure
		success <- formr_run_structure(run_name = "test-run", structure_json_path = tmp_json)
		
		expect_true(success)
		
		# 3. Verify: Fetch structure back to check if the Pause unit exists
		imported <- formr_run_structure("test-run")
		
		# Check that we found the unit we just uploaded
		unit_types <- sapply(imported$units, function(x) x$type)
		expect_true("Pause" %in% unit_types)
	})
})