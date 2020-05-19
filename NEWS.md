# ggcharts 0.2.1

- The themes only set the `plot.title.position` element if a version of `{ggplot2}` >= 3.3.0 is installed. This fixes #75.

# ggcharts 0.2.0

- Two new vignettes: `customize` (#54) and `themes` (#69)
- Two additional datasets: `popeurope` and `popch` (#60)
- The default theme has been tweaked and is now exported as `theme_ggcharts()`
- Additionally, three other new themes have been created: `theme_ng()`, `theme_nightblue()` and `theme_hermit()`
- The default `ggcharts` theme can be changed using `ggcharts_set_theme()`
- `highlight_spec()` has been introduced to enable full customization when highlighting
- The `limit` argument of `bar_chart()`, `lollipop_chart()` and `dumbbell_chart()` has been deprecated in favor of `top_n` (#62)
- `lollipop_chart()` and `bar_chart()` display the number of cases in each distinct value of `x` if the `y` argument is missing (#48)
- `lollipop_chart()` and `bar_chart()` gained an `other` argument. If a threshold is set and `other = TRUE` all categories with values less than the threshold are aggregated and displayed at the bottom of the chart (#67)
- Error messages are thrown using `rlang::abort()` rather than `stop()` (#32)
- A legend used to pop up when the theme of a chart with highlights was changed. This has been fixed in #41
