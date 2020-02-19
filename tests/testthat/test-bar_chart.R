context("Bar Chart")
library(dplyr)
data("biomedicalrevenue")

revenue_2018 <- biomedicalrevenue[biomedicalrevenue$year == 2018, ]
revenue_roche <- biomedicalrevenue[biomedicalrevenue$company == "Roche", ]

vdiffr::expect_doppelganger(
  "Horizontal Sorted Bar Chart",
  bar_chart(revenue_2018, company, revenue)
)

vdiffr::expect_doppelganger(
  "Horizontal Non-Sorted Bar Chart",
  bar_chart(revenue_2018[c(3, 9, 2, 20, 13), ], company, revenue, sort = FALSE)
)

vdiffr::expect_doppelganger(
  "Vertical Non-Sorted Bar Chart",
  bar_chart(revenue_roche, year, revenue, sort = FALSE, horizontal = FALSE)
)

vdiffr::expect_doppelganger(
  "Vertical Sorted Bar Chart",
  bar_chart(revenue_2018, company, revenue, horizontal = FALSE)
)

vdiffr::expect_doppelganger(
  "Horizontal Sorted Bar Chart With Facets",
  bar_chart(biomedicalrevenue, company, revenue, facet = year)
)

# vdiffr::expect_doppelganger(
#   "Vertical Sorted Bar Chart With Facets",
#   bar_chart(biomedicalrevenue, year, revenue, facet = company, horizontal = FALSE)
# )

vdiffr::expect_doppelganger(
  "Horizontal Non-Sorted Bar Chart With Facets",
  bar_chart(biomedicalrevenue, year, revenue, facet = company, sort = FALSE)
)

vdiffr::expect_doppelganger(
  "Vertical Non-Sorted Bar Chart With Facets",
  bar_chart(biomedicalrevenue, year, revenue, facet = company, sort = FALSE, horizontal = FALSE)
)
