# parsnip methods for hierarchical models

multilevelmod allows users to use the parsnip package to fit certain
hierarchical models (e.g., linear, logistic, and Poisson regression).
The package relies on the formula method to specify the random effects.

## Details

As an example, the package includes simulated longitudinal data where
subjects were measured over time points. The outcome was the number of
counts and the predictors are the time point as well as an additional
numeric covariate.

We can fit the model using
[`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html):

    library(tidymodels)
    library(multilevelmod)
    library(poissonreg) # current required for poisson_reg()

    # The lme4 package is required for this model.

    tidymodels_prefer()

    # Split out two subjects to show how prediction works
    data_train <-
      longitudinal_counts |>
      filter(!(subject %in% c("1", "2")))

    data_new <-
      longitudinal_counts |>
      filter(subject %in% c("1", "2"))

    # Fit the model
    count_mod <-
      poisson_reg() |>
      set_engine("glmer") |>
      fit(y ~ time + x + (1 | subject), data = data_train)

    count_mod
    #> parsnip model object
    #>
    #> Generalized linear mixed model fit by maximum likelihood (Laplace
    #>   Approximation) [glmerMod]
    #>  Family: poisson  ( log )
    #> Formula: y ~ time + x + (1 | subject)
    #>    Data: data
    #>       AIC       BIC    logLik -2*log(L)  df.resid
    #>  4474.553  4494.104 -2233.277  4466.553       976
    #> Random effects:
    #>  Groups  Name        Std.Dev.
    #>  subject (Intercept) 0.9394
    #> Number of obs: 980, groups:  subject, 98
    #> Fixed Effects:
    #> (Intercept)         time            x
    #>     -0.5946       1.5145       0.2395

When making predictions, the basic
[`predict()`](https://rdrr.io/r/stats/predict.html) method does the
trick:

    count_mod |> predict(data_new)
    #> # A tibble: 20 x 1
    #>    .pred
    #>    <dbl>
    #>  1  1.19
    #>  2  1.42
    #>  3  1.65
    #>  4  1.83
    #>  5  2.04
    #>  6  2.66
    #>  7  2.96
    #>  8  3.43
    #>  9  3.94
    #> 10  4.64
    #> 11  2.21
    #> 12  2.60
    #> 13  2.97
    #> 14  3.38
    #> 15  4.16
    #> 16  4.90
    #> 17  5.45
    #> 18  6.20
    #> 19  7.55
    #> 20  8.64

## See also

Useful links:

- <https://github.com/tidymodels/multilevelmod>

- <http://multilevelmod.tidymodels.org/>

## Author

**Maintainer**: Hannah Frick <hannah@posit.co>
([ORCID](https://orcid.org/0000-0002-6049-5258))

Authors:

- Max Kuhn <max@posit.co>
  ([ORCID](https://orcid.org/0000-0003-2402-136X))

Other contributors:

- Posit Software, PBC (03wc8by49) \[copyright holder, funder\]
