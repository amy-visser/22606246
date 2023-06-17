Total_Fully_Vaccinated <- function(covid_df) {
    filtered_data <- covid_df %>%
        filter(date == as.Date("2022-05-22")) %>%
        filter(continent == "Africa")

    summary_data <- filtered_data %>%
        group_by(location) %>%
        summarize(people_fully_vaccinated_per_population = sum(people_fully_vaccinated, na.rm = TRUE) / sum(population, na.rm = TRUE))

    ggplot(summary_data, aes(x = people_fully_vaccinated_per_population, y = location, fill = location)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        labs(title = "Percentage of Population Fully Vaccinated in African Countries",
             subtitle = "Information correct as of May 2022",
             x = "Percentage of Population Fully Vaccinated",
             y = "Country") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              legend.position = "none")
}
