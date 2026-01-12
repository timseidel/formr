# Process Results Wrapper

The master pipeline for cleaning data.

## Usage

``` r
formr_post_process_results(
  run_name = NULL,
  item_list = NULL,
  results = NULL,
  item_displays = NULL,
  remove_test_sessions = TRUE,
  tag_missings = !is.null(item_displays)
)
```

## Arguments

- run_name:

  Name of the run (string). If provided, data and structure will be
  fetched automatically.

- item_list:

  Metadata tibble OR a Run Structure list. Required if `run_name` is
  NULL.

- results:

  Results tibble. Required if `run_name` is NULL.

- item_displays:

  Optional display log tibble.

- remove_test_sessions:

  Filter out sessions marked as testing.

- tag_missings:

  Tag NAs if display data is present.
