Monthly_Insights <- function(weather_df) {
    weather_df$date <- parse_date_time(weather_df$date, orders = c("mdy", "ymd", "dmy"))
    weather_df$date <- as.Date(weather_df$date, origin = "1970-01-01")

    weather_df$month <- month(weather_df$date)

    processed_df <- weather_df %>%
        group_by(month) %>%
        summarise(
            na_count = sum(is.na(precipitation)),
            sum_precip = sum(precipitation, na.rm = T),
            dry_days = sum(precipitation == 0, na.rm = T),
            under_five_mm = sum(precipitation > 0 & precipitation < 5, na.rm = T),
            five_to_ten_mm = sum(precipitation >= 5 & precipitation < 10, na.rm = T),
            ten_to_fifteen_mm = sum(precipitation > 10 & precipitation < 15, na.rm = T),
            fifteen_to_twenty_mm = sum(precipitation >= 15 & precipitation < 20, na.rm = T),
            twenty_plus_mm = sum(precipitation >= 20, na.rm = T)
        )

    long_format_processed_df <- gather(
        processed_df,
        precip_mm_range,
        precip_day_count,
        under_five_mm:twenty_plus_mm,
        factor_key = T
    )

    plot <- ggplot(data = long_format_processed_df) +
        geom_point(mapping = aes(x = month, y = precip_day_count, color = precip_mm_range)) +
        labs(
            title = "Precipitation by mm Range, 1979-2020",
            subtitle = "Days with 0mm are excluded",
            y = "Day Count",
            x = "Month"
        ) +
        theme_bw() +
        scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
        scale_colour_discrete(
            name  = "MM Ranges",
            breaks = c(
                "under_five_mm",
                "five_to_ten_mm",
                "ten_to_fifteen_mm",
                "fifteen_to_twenty_mm",
                "twenty_plus_mm"
            ),
            labels = c("<= 5", ">= 5 & < 10", ">= 10 & < 15", ">= 15 & < 20", ">= 20")
        )

    return(list(processed_df = processed_df, plot = plot))
}
