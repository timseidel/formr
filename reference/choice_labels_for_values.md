# Switch choice values with labels

Replaces numeric values with their corresponding text labels based on
[`haven::labelled`](https://haven.tidyverse.org/reference/labelled.html)
attributes or item metadata.

## Usage

``` r
choice_labels_for_values(survey, item_name)
```

## Arguments

- survey:

  A data frame of results.

- item_name:

  The name of the item.

## Value

A character vector of labels.
