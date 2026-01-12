# Get Run Results

Fetches results and converts them into tidy tibbles. Automatically
handles randomization (shuffle) groups by pivoting them into columns.

## Usage

``` r
formr_results(
  run_name,
  surveys = NULL,
  session_ids = NULL,
  item_names = NULL,
  join = FALSE
)
```

## Arguments

- run_name:

  Name of the run.

- surveys:

  Optional. Vector of survey names to fetch.

- session_ids:

  Optional. Filter by specific session IDs.

- item_names:

  Optional. Filter by specific item names (columns).

- join:

  Logical. If TRUE, merges all surveys into one wide table by 'session',
  suffixing column names with the survey name to prevent conflicts.

## Value

A tibble (if joined or only 1 survey found) or a named list of tibbles.
