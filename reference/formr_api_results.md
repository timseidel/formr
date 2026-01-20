# Get and Process Run Results

This is the main function for scientists. It fetches data from the API,
automatically cleans types (dates/numbers), reverses items, computes
scales, and joins everything into one dataframe.

## Usage

``` r
formr_api_results(
  run_name,
  ...,
  compute_scales = TRUE,
  join = TRUE,
  remove_test_sessions = TRUE
)
```

## Arguments

- run_name:

  Name of the run.

- ...:

  Filters passed to API (e.g. `surveys = c("Daily", "Intake")`,
  `session_ids = "..."`).

- compute_scales:

  Logical. Should scales (e.g. `extraversion`) be computed from items
  (e.g. `extra_1`, `extra_2`)?

- join:

  Logical. If TRUE (default), joins all surveys into one wide dataframe.

- remove_test_sessions:

  Logical. Filter out sessions marked as testing?

## Value

A processed tibble (if joined) or a list of processed tibbles.
