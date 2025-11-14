# Simulated longitudinal Poisson counts

Simulated longitudinal Poisson counts

## Value

- longitudinal_counts:

  a tibble

## Details

These are simulated data of 100 subjects each with 10 time points and an
additional numeric covariate. The linear predictor has a random standard
normal intercept per subject, a time coefficient of 1.50, and a
covariate coefficient of 0.25.

## Examples

``` r
data(longitudinal_counts)
str(longitudinal_counts)
#> tibble [1,000 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ subject: Factor w/ 100 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ time   : num [1:1000] 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1 ...
#>  $ x      : num [1:1000] 2.56 2.69 2.68 2.49 2.29 ...
#>  $ y      : int [1:1000] 0 0 3 2 0 1 0 2 3 0 ...
```
