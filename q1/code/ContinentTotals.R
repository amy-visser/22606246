library(knitr)
library(kableExtra)

ContinentTotals <- function(covid_df) {
    selected_continents <- c("Europe", "Asia", "Oceania", "North America", "South America", "Africa")

    result <- covid_df %>%
        filter(continent %in% selected_continents) %>%
        group_by(continent) %>%
        summarise(TotalCases = sum(new_cases, na.rm = TRUE),
                  TotalDeaths = sum(new_deaths, na.rm = TRUE))

    kable(result, caption = "Total Number of Cases and Deaths by Continent", align = "c") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)
}

# Example usage:
ContinentTotals(covid_df)
