library(ggplot2)
library(dplyr)
library(cowplot)
library(lubridate)
library(scales)

Annual_Max_and_Min <- function(weather_df){

    weather_df$date <- parse_date_time(weather_df$date, orders = c("mdy", "ymd", "dmy"))
    weather_df$date <- as.Date(weather_df$date, origin = "1970-01-01")

    pastel_colors <- c("palevioletred", "steelblue")

    a <- weather_df %>%
        filter(date >= "2010-01-01") %>%
        group_by(date) %>%
        summarise(year_mean = mean(max_temp)) %>%
        ggplot(aes(date, year_mean)) +
        geom_point(color = pastel_colors[1]) +
        geom_line(color = pastel_colors[1]) +
        geom_smooth(method = "loess", color = pastel_colors[2]) +
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 year") +  # Format and breaks for the x-axis
        theme(axis.title.x = element_blank(),
              legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Annual mean maximum temperature", subtitle = "UK: 2010 - 2020", y = "Degrees Celsius")

    b <- weather_df %>%
        filter(date >= "2010-01-01") %>%
        group_by(date) %>%
        summarise(year_mean = mean(min_temp)) %>%
        ggplot(aes(date, year_mean)) +
        geom_point(color = pastel_colors[1]) +
        geom_line(color = pastel_colors[1]) +
        geom_smooth(method = "loess", color = pastel_colors[2]) +
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 year") +  # Format and breaks for the x-axis
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Annual mean minimum temperature", subtitle = "UK: 2010 - 2020")

    g <- plot_grid(a, NULL, b, nrow = 1, rel_widths = c(1, 0.05, 1))

    return(g)
}
