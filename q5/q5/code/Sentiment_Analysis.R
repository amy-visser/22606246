library(ggplot2)

Sentiment_Analysis <- function(googlereview_df) {
    filtered_data <- googlereview_df %>%
        filter(Sentiment %in% c("Positive", "Negative", "Neutral"))

    ggplot(filtered_data, aes(x = Sentiment)) +
        geom_bar(fill = c("palevioletred", "olivedrab3", "steelblue"), alpha = 0.8) +
        labs(title = bquote("Number of Positive, Negative, and Neutral Reviews"),
             subtitle = "Data obtained from Google Play Reviews file",
             x = "Sentiment",
             y = "Count") +
        theme_bw() +
        theme(plot.title = element_text(face = "bold"))
}

Sentiment_Analysis(googlereview_df)
