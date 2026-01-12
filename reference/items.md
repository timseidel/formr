# Get item metadata from survey results

Extracts the item metadata (type, label, choices) attached to the
columns of a results data frame by `formr_recognise`.

## Usage

``` r
items(survey)
```

## Arguments

- survey:

  A data frame of results (processed by `formr_recognise`).

## Value

A tibble containing metadata for all items found.
