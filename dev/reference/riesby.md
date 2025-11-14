# Imipramine longitudinal data

Imipramine longitudinal data

## Source

Reisby, N., Gram, L.F., Bech, P. et al. Imipramine: Clinical effects and
pharmacokinetic variability. Psychopharmacology 54, 263-272 (1977).

## Value

- riesby:

  a tibble

## Details

These data are from a longitudinal clinical trial for depression. The
outcome is the change in depression scores week-to-week. The
`endogenous` column is an indicator for whether the subject fit the WHO
Depression Scale classification of endogenous. The `imipramine` and
`desipramine` columns are measurements of plasma levels for both
substances.

## Examples

``` r
data(riesby)
str(riesby)
#> tibble [250 × 7] (S3: tbl_df/tbl/data.frame)
#>  $ subject    : Factor w/ 66 levels "101","103","104",..: 1 1 1 1 2 2 2 2 3 3 ...
#>  $ depr_score : num [1:250] -8 -19 -22 -23 -18 -9 -18 -20 -11 -16 ...
#>  $ week       : num [1:250] 0 1 2 3 0 1 2 3 0 1 ...
#>  $ male       : num [1:250] 0 0 0 0 1 1 1 1 1 1 ...
#>  $ endogenous : num [1:250] 0 0 0 0 0 0 0 0 1 1 ...
#>  $ imipramine : num [1:250] 4.04 3.93 4.33 4.37 2.77 ...
#>  $ desipramine: num [1:250] 4.2 4.81 4.96 4.96 5.24 ...
```
