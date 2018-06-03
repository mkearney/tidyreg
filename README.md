
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyreg <img src="man/figures/logo.png" width="160px" align="right" />

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

🎓 Tidy tools for
academics

## \*\*\* This package is in very early development. Feedback is encouraged\!\!\! \*\*\*

## Installation

<!-- You can install the released version of tidyreg from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tidyreg")
```
-->

Install the development version from
[Github](https://github.com/mkearney/tidyreg) with:

``` r
## install devtools if not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
## install tidyreg from Github
devtools::install_github("mkearney/tidyreg")
```

Load the package (it, of course, plays nicely with tidyverse).

``` r
## load tidyverse
library(tidyverse)

## load tidyreg
library(tidyreg)
```

## Regression models

### Ordinary Least Squares (OLS)

Conduct an Ordinary Least Squares (OLS) regression analysis.

``` r
polcom %>%
  tidy_regression(follow_trump ~ news_1 + ambiv_sexism_1) %>%
  tidy_summary()
#> # A tidy model
#> Model formula  : follow_trump ~ news_1 + ambiv_sexism_1
#> Model type     : Ordinary Least Squares (OLS) regression
#> Model pkg::fun : stats::lm()
#> Model data     : 243 (observations) X 3 (variables)
#> $fit
#> fit_stat     n     df    estimate    p.value  stars
#> F          243      2      3.831      0.023   *
#> R^2        243      -      0.031       -         
#> Adj R^2    243      -      0.023       -         
#> RMSE       243      -      0.409       -         
#> AIC        243      -    260.148       -         
#> BIC        243      -    274.121       -         
#> 
#> $coef
#> term               est     s.e.    est.se    p.value  stars   std.est
#> (Intercept)      0.745    0.097     7.692      <.001   ***      <.001
#> news_1           0.022    0.012     1.811      0.071   +        0.048
#> ambiv_sexism_1  -0.038    0.021    -1.870      0.063   +       -0.050
```

### Logistic (dichotomous)

Conduct a logistic regression analysis for binary (dichotomous)
outcomes.

``` r
polcom %>%
  tidy_regression(follow_trump ~ news_1 + ambiv_sexism_1, type = "logistic") %>%
  tidy_summary()
#> # A tidy model
#> Model formula  : follow_trump ~ news_1 + ambiv_sexism_1
#> Model type     : Logistic regression
#> Model pkg::fun : stats::glm()
#> Model data     : 243 (observations) X 3 (variables)
#> $fit
#> fit_stat           n     df    estimate    p.value  stars
#> χ2               243    240    247.442      0.357      
#> Δχ2              243      2      7.466      0.024   *
#> Nagelkerke R^2   243      -      0.030       -         
#> McFadden R^2     243      -      0.029       -         
#> RMSE             243      -      2.540       -         
#> AIC              243      -    253.442       -         
#> BIC              243      -    263.921       -         
#> 
#> $coef
#> term               est     s.e.    est.se    p.value  stars   std.est
#> (Intercept)      1.133    0.553     2.049      0.040   *        <.001
#> news_1           0.127    0.070     1.808      0.071   +        0.195
#> ambiv_sexism_1  -0.229    0.122    -1.872      0.061   +       -0.201
```

### Poisson (count)

Conduct a poisson regression analysis for count data.

``` r
polcom %>%
  mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, type = "poisson") %>%
  tidy_summary()
#> # A tidy model
#> Model formula  : polarize ~ news_1 + ambiv_sexism_1
#> Model type     : Poisson regression
#> Model pkg::fun : stats::glm()
#> Model data     : 242 (observations) X 3 (variables)
#> $fit
#> fit_stat           n     df    estimate    p.value  stars
#> χ2               242    239   6549.419      <.001   ***
#> Δχ2              242      2    399.077      <.001   ***
#> Nagelkerke R^2   242      -      0.808       -         
#> McFadden R^2     242      -      0.057       -         
#> RMSE             242      -      0.760       -         
#> AIC              242      -   7725.222       -         
#> BIC              242      -   7735.689       -         
#> 
#> $coef
#> term               est     s.e.     est.se    p.value  stars   std.est
#> (Intercept)      3.798    0.038     99.448      <.001   ***      <.001
#> news_1           0.045    0.005      9.358      <.001   ***      0.881
#> ambiv_sexism_1  -0.126    0.008    -15.852      <.001   ***     -2.230
```

### Negative binomial (overdispersed)

Conduct a negative binomial regression analysis for overdispersed count
data.

``` r
polcom %>%
  mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, type = "negbinom") %>%
  tidy_summary()
#> # A tidy model
#> Model formula  : polarize ~ news_1 + ambiv_sexism_1
#> Model type     : Negative binomial regression
#> Model pkg::fun : MASS::glm.nb()
#> Model data     : 242 (observations) X 3 (variables)
#> $fit
#> fit_stat           n     df    estimate    p.value  stars
#> χ2               242    239    293.328      0.009   **
#> Δχ2              242      2      8.440      0.015   *
#> Nagelkerke R^2   242      -      0.034       -         
#> McFadden R^2     242      -      0.028       -         
#> RMSE             242      -      0.761       -         
#> AIC              242      -   2312.391       -         
#> BIC              242      -   2326.347       -         
#> 
#> $coef
#> term               est     s.e.    est.se    p.value  stars   std.est
#> (Intercept)      3.741    0.258    14.510      <.001   ***      3.752
#> news_1           0.053    0.032     1.632      0.103            0.113
#> ambiv_sexism_1  -0.123    0.054    -2.273      0.023   *       -0.158
```

### Robust and quasi- models

``` r
polcom %>%
  mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, 
    type = "quasipoisson", robust = TRUE) %>%
  tidy_summary()
#> # A tidy model
#> Model formula  : polarize ~ news_1 + ambiv_sexism_1
#> Model type     : [Robust] Poisson regression
#> Model pkg::fun : robust::glmRob()
#> Model data     : 242 (observations) X 3 (variables)
#> $fit
#> fit_stat           n     df     estimate    p.value  stars
#> χ2               242    239    6989.543      <.001   ***
#> Δχ2              242      2   58782.937      <.001   ***
#> Nagelkerke R^2   242      -       1.000       -         
#> McFadden R^2     242      -       0.894       -         
#> RMSE             242      -      31.865       -         
#> AIC              242      -    2245.147       -         
#> BIC              242      -    2259.103       -         
#> 
#> $coef
#> term               est     s.e.     est.se    p.value  stars   std.est
#> (Intercept)      3.705    0.071     51.968      <.001   ***      <.001
#> news_1           0.079    0.010      8.325      <.001   ***      1.279
#> ambiv_sexism_1  -0.241    0.022    -11.179      <.001   ***     -2.086
```
