library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

BarPlot <- function(weather_df) {
    df_barplot <- weather_df %>%
        mutate(year = as.Date(as.character(date), format = "%Y%m%d")) %>%
        filter(year >= as.Date("1995-01-01") & year <= as.Date("2020-12-31")) %>%
        drop_na(precipitation) %>%
        group_by(year) %>%
        summarize(max_sun = max(sunshine), max_precipitation = max(precipitation)) %>%
        ungroup() %>%
        mutate(max_sun = max_sun / max(max_sun), max_precipitation = max_precipitation / max(max_precipitation)) %>%
        gather(Type, Value, -year) %>%
        mutate(year = as.Date(paste0(year, "-01-01")))

    df_barplot %>%
        ggplot() +
        geom_bar(aes(x = year, y = Value, fill = Type), stat = "identity") +
        facet_wrap(~Type, scales = "free_y", nrow = 2) +
        scale_x_date(labels = scales::date_format("%b '%y"), date_breaks = "2 years") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "", y = "") +
        guides(fill = FALSE)
}
