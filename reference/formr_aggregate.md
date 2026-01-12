# Aggregate Scales

Calculates row means for items sharing a common stem. E.g. `bfi_n_1`,
`bfi_n_2` -\> `bfi_n`.

## Usage

``` r
formr_aggregate(results, item_list = NULL, min_items = 2)
```

## Arguments

- results:

  Results tibble.

- item_list:

  Metadata tibble (optional, for checking types).

- min_items:

  Minimum items required to form a scale (default 2).
