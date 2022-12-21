
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bis620.2022

<!-- badges: start -->

[![R-CMD-check](https://github.com/wanjingc7/bis620.2022/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wanjingc7/bis620.2022/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/wanjingc7/bis620.2022/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/wanjingc7/bis620.2022/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/wanjingc7/bis620.2022/branch/main/graph/badge.svg)](https://app.codecov.io/gh/wanjingc7/bis620.2022?branch=main)
[![lint](https://github.com/wanjingc7/bis620.2022/actions/workflows/lint.yaml/badge.svg)](https://github.com/wanjingc7/bis620.2022/actions/workflows/lint.yaml)
<!-- badges: end -->

The goal of bis620.2022 is to

For the Accelerometry data:

1.  Capture spectral signature of Accelerometry data with three
    dimensions using Fast Fourier Transform (FFT) and also calculate
    frequency with time.

2.  Create a visualization for movements in three dimensions with time
    or frequency.

For the clinical trials data of Panitumumab on head-neck cancer
patients:

1.  Create histograms of distribution of background information of the
    participants, including age, sex, race, diagnosis (with or without
    stages).

2.  Create histograms of distribution of the treatment outcome: adverse
    events and best treatment response.

3.  Conduct logistic regressions to test whether a certain factor is
    prognostic or predictive.

## Links to coverage and Lint Results

[Links to
Coverage](https://github.com/wanjingc7/bis620.2022/actions/workflows/test-coverage.yaml)

[Links to Lint
Results](https://github.com/wanjingc7/bis620.2022/actions/workflows/lint.yaml)

## Installation

You can install the development version of bis620.2022 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("wanjingc7/bis620.2022")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(bis620.2022)
## basic example code

#to get FFT of dimensions and frequency from time
data("ukb_accel")
ukb_accel |> spectral_signature()
#> # A tibble: 540,000 × 4
#>          X      Y       Z      freq
#>      <dbl>  <dbl>   <dbl>     <dbl>
#>  1  78038. 28208. 753298. 0.0000913
#>  2 153932. 98732.  53521. 0.0000913
#>  3  43385. 58240.  66251. 0.0000913
#>  4  53004. 50652.  10922. 0.0000913
#>  5  97164. 76529.  59418. 0.0000913
#>  6  30740. 49130. 101001. 0.0000913
#>  7 116648. 58509.  84698. 0.0000913
#>  8  43271. 55604.  39442. 0.0000913
#>  9  63228. 73288.  46781. 0.0000913
#> 10  56477. 17581.  70102. 0.0000913
#> # … with 539,990 more rows

#to create visualization of movement with time
ukb_accel |> accel_plot()
```

<img src="man/figures/README-example Accelerometry data-1.png" width="100%" />

``` r

#to create visualization of logarithm transformed FFT
#movement with frequency
ukb_accel |> spectral_signature(take_log = T) |> accel_plot()
```

<img src="man/figures/README-example Accelerometry data-2.png" width="100%" />

``` r
#only a few examples shown here, to see full list of examples, please
#check the vignette.

data("full_data")

#plot age distribution of participants
plot_age(df = full_data, subject_id_col = "SUBJID", age_col = "AGE",
         treat_col = "ATRT", age_interval = 5)
#> Warning: `stat(count)` was deprecated in ggplot2 3.4.0.
#> ℹ Please use `after_stat(count)` instead.
#> ℹ The deprecated feature was likely used in the bis620.2022 package.
#>   Please report the issue to the authors.
```

<img src="man/figures/README-example Clinical Trials data-1.png" width="100%" />

``` r


#plot adverse events distribution of participants
get_ae(df = full_data, ae_col = "AETERM", treat_col = "ATRT", top = 30)
#> [[1]]
#> # A tibble: 37 × 3
#>    AETERM         Chemotherapy `panit. plus chemotherapy`
#>    <chr>                 <int>                      <int>
#>  1 NAUSEA                  461                        525
#>  2 NEUTROPENIA             434                        532
#>  3 VOMITING                258                        370
#>  4 ANEMIA                  248                        377
#>  5 FATIGUE                 186                        262
#>  6 ANOREXIA                179                        185
#>  7 MUCOSITIS               155                        160
#>  8 STOMATITIS              150                         92
#>  9 CONSTIPATION            137                        245
#> 10 HYPOMAGNESEMIA          137                        307
#> # … with 27 more rows
#> 
#> [[2]]
```

<img src="man/figures/README-example Clinical Trials data-2.png" width="100%" />

``` r

#conduct a logstic regression to see if age is prognostic
prognostic_var(df = full_data, subject_id_col = "SUBJID", treat_col = "ATRT",
               control_val = "Chemotherapy", death_col = "DEATH_FLAG", 
               var_col = "AGE")
#> 
#> Call:  glm(formula = as.factor(death_vector) ~ var_vector, family = binomial(link = "logit"))
#> 
#> Coefficients:
#> (Intercept)   var_vector  
#>    -0.08529      0.02386  
#> 
#> Degrees of Freedom: 259 Total (i.e. Null);  258 Residual
#> Null Deviance:       270.9 
#> Residual Deviance: 269.4     AIC: 273.4

#conduct a logstic regression to see if age is predictive
predictive_var(df = full_data, subject_id_col = "SUBJID",
               death_col = "DEATH_FLAG", treat_col = "ATRT", var_col = "AGE",
               interaction = T)
#> 
#> Call:  glm(formula = as.factor(death_vector) ~ var_vector * as.factor(treat_vector), 
#>     family = binomial(link = "logit"))
#> 
#> Coefficients:
#>                                                (Intercept)  
#>                                                   -0.08529  
#>                                                 var_vector  
#>                                                    0.02386  
#>            as.factor(treat_vector)panit. plus chemotherapy  
#>                                                    1.40845  
#> var_vector:as.factor(treat_vector)panit. plus chemotherapy  
#>                                                   -0.01771  
#> 
#> Degrees of Freedom: 519 Total (i.e. Null);  516 Residual
#> Null Deviance:       500.4 
#> Residual Deviance: 496   AIC: 504
```
