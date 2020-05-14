ggcharts
================

[![R build status](https://github.com/thomas-neitmann/ggcharts/workflows/R-CMD-check/badge.svg)](https://github.com/thomas-neitmann/ggcharts/actions) [![CRAN Version](https://www.r-pkg.org/badges/version/ggcharts?color=green)](https://cran.r-project.org/package=ggcharts) [![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/ggcharts?color=green)](https://cran.r-project.org/package=ggcharts) [![Lifecycle Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Overview
--------

`{ggcharts}` provides a high-level `{ggplot2}` interface for creating common charts. Its aim is both simple and ambitious: to get you from your data visualization idea to an actual plot faster. How so? By taking care of a lot of data preprocessing, obscure `{ggplot2}` details and plot styling for you. The resulting plots are `ggplot` objects and can be further customized using any `{ggplot2}` function.

Installation
------------

The package is available from CRAN.

``` r
install.packages("ggcharts")
```

Alternatively, you can install the latest development version from GitHub.

``` r
if (!"remotes" %in% installed.packages()) {
  install.packages("remotes")
}
remotes::install_github("thomas-neitmann/ggcharts", upgrade = "never")
```

If you get an error when trying to install from GitHub, run this code and then try to install once again.

``` r
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")
```

If the installation still fails please open an [issue](https://github.com/thomas-neitmann/ggcharts/issues).

Why ggcharts?
-------------

Thanks to `{ggplot2}` you can create beautiful plots in `R`. However, it can often take quite a bit of effort to get from a data visualization idea to an actual plot. As an example, let's say you want to create a faceted bar chart displaying the top 10 within each facet ordered from highest to lowest. What sounds simple is actually pretty hard to achieve. Have a look:

``` r
library(dplyr)
library(ggplot2)
library(ggcharts)
data("biomedicalrevenue")

biomedicalrevenue %>%
  filter(year %in% c(2012, 2015, 2018)) %>%
  group_by(year) %>%
  top_n(10, revenue) %>%
  ungroup() %>%
  mutate(company = tidytext::reorder_within(company, revenue, year)) %>%
  ggplot(aes(company, revenue)) +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  facet_wrap(vars(year), scales = "free_y")
```

<img src="man/figures/README-motivation-1.png" width="100%" />

That's a lot of code! And you likely never heard of some of the functions involved. With `{ggcharts}` you can create the same plot (actually an even better looking one) in almost a single line of code.

``` r
biomedicalrevenue %>%
  filter(year %in% c(2012, 2015, 2018)) %>%
  bar_chart(company, revenue, facet = year, top_n = 10)
```

<img src="man/figures/README-motivation_continued-1.png" width="100%" />

Features
--------

### Charts

-   `bar_chart()`
-   `diverging_bar_chart()`
-   `column_chart()`
-   `lollipop_chart()`
-   `diverging_lollipop_chart()`
-   `dumbbell_chart()`
-   `pyramid_chart()`

### Themes

-   `theme_ggcharts()`
-   `theme_ng()`
-   `theme_nightblue()`
-   `theme_hermit()`
