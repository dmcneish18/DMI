
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dynamic

<!-- badges: start -->
<!-- badges: end -->

The goal of dynamic measurement invariance cutoffs (DMI) is to simulate
fit index difference for two-group factor analysis models. The primary
goal is to generalize the procedure used by [Cheung & Rensvold
(2002)](https://www.tandfonline.com/doi/abs/10.1207/S15328007SEM0902_5),
who first introduced fit index cutoffs that are commonly used but have
recently been noted to have limited generalizability. The app uses
re-simulates cutoffs with a procedure similar to Cheung and Rensvold
(2002), but the cutoffs tailored to the user’s model and data
characteristics

## Run the Shiny App Locally

A web-based version of the app can be viewed
[here](https://www.dynamicfit.app/MI). The app can also be run on your
local machine to potentially increase speed or if the website is down
for periodic maintenance. The Shiny app can be called from RStudio using
the following code,

``` r
list.of.packages <- c("shiny","lavaan", "dplyr","MASS","shinyjs","rmarkdown","knitr","tidyr","purrr","GenOrd","tinytex","semPlot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

shiny::runGitHub("DMI","dmcneish18")
```

## Tutorial on Using the App

A separate document located [here](https://osf.io/2apbg) walks through
how to use the app, how to interpret the output, and what options are
currently available.
