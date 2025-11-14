# Measurement systems analysis data

Measurement systems analysis data

## Value

- msa_data:

  a tibble

## Details

A biological assay (i.e. a lab test) was run on 56 separate samples
twice. The goal is to measure what percentage of the total variation in
the results is related to the measurement system and how much is
attributable to the true systematic difference (sample-to-sample).

## Examples

``` r
data(msa_data)
str(msa_data)
#> tibble [112 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ id       : chr [1:112] "sample_001" "sample_001" "sample_002" "sample_002" ...
#>  $ replicate: chr [1:112] "rep_1" "rep_2" "rep_1" "rep_2" ...
#>  $ value    : num [1:112] 2.406 1.837 -0.674 -0.102 1.08 ...
```
