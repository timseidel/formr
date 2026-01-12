# Reverse Items

Reverses items ending in 'R' (e.g. `extra_1R`) using metadata bounds.
Uses the robust formula: (Max + Min) - Value.

## Usage

``` r
formr_reverse(results, item_list = NULL)
```

## Arguments

- results:

  Results tibble.

- item_list:

  Metadata tibble.
