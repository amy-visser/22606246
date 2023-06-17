Precipitation <- function(processed_df) {

long_format_processed_df <- gather(
    processed_df,
    precip_mm_range,
    precip_day_count,
    under_five_mm:twenty_plus_mm,
    factor_key = T
)

ggplot(data = long_format_processed_df) +
    geom_point(mapping = aes(x = month, y = precip_day_count, color = precip_mm_range)) +
    labs(
        title = "Precipitation by mm Range",
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
}