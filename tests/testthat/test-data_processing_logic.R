library(testthat)
library(jsonlite)
library(tibble)
library(haven)
library(formr)

# ---------------------------------------------------------
# UTILITY TESTS
# ---------------------------------------------------------

test_that("random_date_in_range generates valid dates", {
	lower <- "2020/01/01"
	upper <- "2020/12/31"
	n <- 100
	
	dates <- random_date_in_range(n, lower, upper)
	
	expect_equal(length(dates), n)
	expect_true(all(dates >= as.POSIXct(lower) & dates <= as.POSIXct(upper)))
	expect_true(all(diff(dates) >= 0))
})

test_that("email_image generates correct CID format", {
	result <- email_image("plot_123")
	expect_match(result, "^cid:[a-zA-Z0-9]+\\.png$")
	
	result <- email_image("plot_123", ext = ".jpg")
	expect_match(result, "^cid:[a-zA-Z0-9]+\\.jpg$")
	
	expect_equal(attr(result, "link"), "plot_123")
})

test_that("items and item functions work correctly", {
	test_df <- data.frame(x = 1:3, y = letters[1:3])
	
	# item_meta requires choices to be a list-column (list inside a list)
	attr(test_df$x, "item_meta") <- list(
		name = "x",
		label = "Test variable",
		type = "number"
	)
	
	item_list <- items(test_df)
	
	expect_s3_class(item_list, "tbl_df")
	expect_equal(nrow(item_list), 1)
	expect_equal(item_list$name, "x")
	
	item_info <- item(test_df, "x")
	expect_equal(item_info$name, "x")
	expect_equal(item_info$label, "Test variable")
	
	expect_error(item(test_df, "z"), "not found")
})

test_that("choice_labels_for_values works correctly", {
	test_df <- data.frame(q1 = c(1, 2, 1))
	
	# Choices should be Label = Value (e.g. Agree = 1)
	# This enables the helper to map Value (1) -> Label (Agree)
	attr(test_df$q1, "item_meta") <- list(
		name = "q1",
		choices = list(list(
			"Agree" = 1,
			"Disagree" = 2
		))
	)
	
	result <- choice_labels_for_values(test_df, "q1")
	expect_equal(result, c("Agree", "Disagree", "Agree"))
})

test_that("formr_label_missings handles missing values correctly", {
	skip("Feature currently disabled in formr_label_missings")
})

# ---------------------------------------------------------
# LOGIC TESTS (using local JSON files)
# ---------------------------------------------------------

test_that("formr_post_process_results works correctly", {
	results <- jsonlite::fromJSON(txt = 
																	system.file('extdata/BFI_post.json', package = 'formr', mustWork = TRUE))
	
	#Extract $items from the loaded JSON wrapper
	json_items <- jsonlite::fromJSON(
		system.file('extdata/BFI_post_items.json', package = 'formr', mustWork = TRUE)
	)
	items <- tibble::as_tibble(json_items$items)
	
	item_displays <- jsonlite::fromJSON(
		system.file('extdata/BFI_post_itemdisplay.json', package = 'formr', mustWork = TRUE))
	
	# Removed legacy arguments
	processed_results <- formr_post_process_results(
		item_list = items, 
		results = results, 
		item_displays = item_displays,
		tag_missings = FALSE 
	)
	
	if("session" %in% names(results)) {
		expect_true(all(!stringr::str_detect(processed_results$session, "XXX")))
	}
	
	if("created" %in% names(processed_results)) {
		expect_true(inherits(processed_results$created, "POSIXct"))
	}
})

test_that("formr_aggregate works with example data", {
	results <- jsonlite::fromJSON(txt = 
																	system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
	
	# Extract $items
	json_items <- jsonlite::fromJSON(
		system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE)
	)
	items <- tibble::as_tibble(json_items$items)
	
	results <- formr_recognise(item_list = items, results = results)
	
	agg <- formr_aggregate(
		item_list = items, 
		results = results, 
		min_items = 2
	)
	
	expect_true(all(c('religiousness', 'prefer') %in% names(agg)))
	expect_true(is.numeric(agg$religiousness))
	expect_true(is.numeric(agg$prefer))
	expect_equal(nrow(agg), nrow(results))
})

test_that("formr_recognise works with example data", {
	results <- jsonlite::fromJSON(txt = 
																	system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
	
	# Extract $items
	json_items <- jsonlite::fromJSON(
		system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE)
	)
	items <- tibble::as_tibble(json_items$items)
	
	recognized <- formr_recognise(item_list = items, results = results)
	
	expect_true(inherits(recognized$created, "POSIXct"))
	
	# Check choice processing
	choice_based_items <- sapply(items$choices, function(x) !is.null(x))
	
	if(any(choice_based_items)) {
		item_idx <- which(choice_based_items)[1]
		item_name <- items$name[item_idx]
		
		if(item_name %in% names(recognized)) {
			expect_true(haven::is.labelled(recognized[[item_name]]))
		}
	}
})

test_that("formr_items helper (json loading) works with example data", {
	# Extract $items
	json_items <- jsonlite::fromJSON(
		system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE)
	)
	items_df <- tibble::as_tibble(json_items$items)
	
	expect_true(all(c("name", "type", "label") %in% names(items_df)))
})