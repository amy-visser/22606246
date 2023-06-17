library(ggplot2)
library(dplyr)
library(plotly)

Excess_Mortality <- function(covid_df) {
    filtered_data <- covid_df %>%
        filter(date == as.Date("2021-12-31"))

    filtered_data <- filtered_data %>%
        mutate(covid_mortality_quintile = ntile(total_deaths_per_million, 5),
               covid_mortality_quintile = factor(covid_mortality_quintile)) %>%
        filter(!is.na(covid_mortality_quintile))  # Remove rows with 'NA' in covid_mortality_quintile

    mortal_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")

    age_sp <- filtered_data %>%
        ggplot(aes(x = median_age, y = total_deaths_per_million,
                   color = covid_mortality_quintile, text = location)) +
        geom_point(alpha = 6/10, size = 5) +  # Adjust the size of the dots
        scale_size_continuous(range = c(5, 20)) +
        labs(title = "COVID-19 Mortality Rate by Median Age",
             subtitle = "Countries' total deaths per million on 2021/12/31",
             color = "Mortality Quintile") +
        scale_color_manual(values = mortal_palette) +
        theme_bw() +
        theme(axis.title = element_text())

    return(age_sp)
}

# Call the function
Excess_Mortality(covid_df)
