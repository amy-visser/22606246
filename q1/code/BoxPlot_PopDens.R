library(ggplot2)

Boxplot_PopDens <- function(covid_df) {
    # Log-transform the death rate
    covid_df$log_death_rate <- log(covid_df$total_deaths_per_million)

    # Create boxplots with population densities categorized
    covid_df$pop_density_category <- cut(covid_df$population_density,
                                         breaks = c(0, 100, 500, 1000, Inf),
                                         labels = c("Very Low", "Low", "High", "Very High"))

    # Filter out rows with NA population density
    filtered_df <- covid_df[!is.na(covid_df$pop_density_category), ]

    # Define custom colors for the boxplots
    boxplot_colors <- c("darkolivegreen3", "deepskyblue3", "chocolate2", "goldenrod1")

    # Create the boxplot
    ggplot(filtered_df, aes(x = pop_density_category, y = log_death_rate, fill = pop_density_category)) +
        geom_boxplot(color = "black", outlier.shape = NA) +
        scale_fill_manual(values = boxplot_colors) +
        ggtitle("Boxplots of COVID Death Rate by Population Density") +
        labs(subtitle = "Data sourced from Our World in Data",
             x = "Population Density Category",
             y = "Log-Transformed Death Rate") +
        theme_bw()
}

# Example usage:
Boxplot_PopDens(covid_df)
