# Tag Missing Values (Placeholder)

This function is currently disabled/under construction.

## Usage

``` r
formr_label_missings(results, item_displays)
```

## Arguments

- results:

  Results tibble.

- item_displays:

  Display log tibble.

## Details

Uses `item_displays` log to distinguish between:

- **Shown but skipped** (User saw it, didn't answer)

- **Hidden** (Logic skipped it)

- **Not Reached** (User quit before this page)
