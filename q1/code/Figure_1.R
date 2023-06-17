Figure_1 <- function(covid_df) {
    selected_continents <- c("Asia", "Europe", "Africa", "North America", "South America", "Oceania")

    # Convert date column to proper date format
    covid_df$date <- as.Date(covid_df$date)

    # Filter out missing values in the date and total_cases columns
    covid_df <- covid_df[complete.cases(covid_df$date, covid_df$total_cases), ]

    covid_df %>%
        filter(continent %in% selected_continents) %>%
        group_by(continent, date) %>%
        summarise(total_cases = sum(total_cases)) %>%
        ggplot() +
        geom_line(aes(x = date, y = total_cases, colour = continent), size = 1.25) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Total Cases of Covid per Continent over Time", x = "Date", y = "Total Cases")

}

