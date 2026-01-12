# Aggregate variables and document construction

Computes a row-wise aggregate (mean, sum) and attaches metadata about
which items were included. Useful for transparency in codebooks.

## Usage

``` r
aggregate_and_document_scale(items, fun = rowMeans, stem = NULL)
```

## Arguments

- items:

  A data.frame/tibble of the items to aggregate.

- fun:

  Aggregation function (default: `rowMeans`).

- stem:

  Optional manual stem name. If NULL, auto-detects longest common
  prefix.

## Value

A numeric vector with attributes `scale_item_names` and `label`.

## Examples

``` r
df <- data.frame(e1 = 1:5, e2 = 5:1, e3 = 1:5)
scale_score <- aggregate_and_document_scale(df)
attr(scale_score, "label")
#> [1] "3 e items aggregated by rowMeans"
```
