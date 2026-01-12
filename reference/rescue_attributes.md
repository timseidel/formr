# Rescue lost attributes

Copies attributes (labels, SPSS formats) from one data frame to another.
Useful after `dplyr` operations that strip attributes.

## Usage

``` r
rescue_attributes(df_target, df_source)
```

## Arguments

- df_target:

  The data frame missing attributes.

- df_source:

  The reference data frame containing attributes.

## Value

`df_target` with restored attributes.
