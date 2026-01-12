# Reverse labelled values

Reverses the underlying values for a numeric
[`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
vector while preserving and updating the value labels correctly. Useful
for reversing Likert scales (e.g., 1=Strongly Disagree -\> 5=Strongly
Agree).

## Usage

``` r
reverse_labelled_values(x)
```

## Arguments

- x:

  A numeric vector, potentially with
  [`haven::labelled`](https://haven.tidyverse.org/reference/labelled.html)
  attributes.

## Value

A vector of the same class with values reversed.

## Examples

``` r
x <- haven::labelled(rep(1:3, each = 3), c(Bad = 1, Good = 5))
x
#> <labelled<integer>[9]>
#> [1] 1 1 1 2 2 2 3 3 3
#> 
#> Labels:
#>  value label
#>      1   Bad
#>      5  Good
reverse_labelled_values(x)
#> <labelled<double>[9]>
#> [1] 5 5 5 4 4 4 3 3 3
#> 
#> Labels:
#>  value label
#>      1  Good
#>      5   Bad
```
