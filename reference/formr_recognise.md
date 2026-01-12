# Recognise data types based on survey metadata

Applies type conversion (factors, dates, numbers) and attributes
(labels) to the results table based on the survey structure.

## Usage

``` r
formr_recognise(item_list, results)
```

## Arguments

- item_list:

  Metadata tibble from
  [`formr_survey_structure()`](http://rubenarslan.github.io/formr/reference/formr_survey_structure.md).

- results:

  Results tibble from
  [`formr_results()`](http://rubenarslan.github.io/formr/reference/formr_results.md).

## Value

The processed results tibble with correct types and attributes.
