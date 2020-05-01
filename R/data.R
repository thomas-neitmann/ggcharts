#' Top Biomedical Companies Revenues.
#'
#' Annual revenues of top biomedical companies from 2011 to 2018.
#'
#' @format A data frame with 224 rows and 3 variables:
#' \describe{
#'   \item{company}{Name of the company}
#'   \item{year}{Fiscal year}
#'   \item{revenue}{Revenue in billion USD}
#' }
#' @source \url{https://en.wikipedia.org/wiki/List_of_largest_biomedical_companies_by_revenue}
"biomedicalrevenue"

#' Population Statistics of Switzerland
#'
#' Swiss population in 2020 by five-year age groups
#'
#' @format A data frame with 42 rows and 3 variables:
#' \describe{
#'   \item{age}{Five-year age group}
#'   \item{sex}{Sex}
#'   \item{pop}{Population}
#' }
#' @source US Census International Data Base
"popch"

#' European Population
#'
#' Population of European countries in 1952 and 2007
#'
#' @format A data frame with 30 rows and 3 variables:
#' \describe{
#'   \item{country}{Name of the country}
#'   \item{pop1952}{Population in 1952 (in millions)}
#'   \item{pop2007}{Population in 2007 (in millions)}
#' }
#' @source \url{http://www.gapminder.org/data/}
popeurope <- gapminder::gapminder %>%
  dplyr::filter(year %in% c(1952, 2007), continent == "Europe") %>%
  dplyr::mutate(pop = pop / 1e6) %>%
  tidyr::pivot_wider(
    id_cols = country,
    values_from = pop,
    names_from = year,
    names_prefix = "pop"
  ) %>%
  dplyr::mutate(country = as.character(country)) %>%
  as.data.frame()
save(popeurope, file = "data/popeurope.rda")
