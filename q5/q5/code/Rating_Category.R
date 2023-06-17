library(ggplot2)

Rating_Category <- function(googleclean_df) {
    ggplot(data = googleclean_df, aes(x = Category, y = Rating)) +
        geom_boxplot() +
        stat_boxplot(geom = "errorbar") +
        stat_summary(fun.y = mean, col = "orange2", geom = "point", size = 3) +
        ggtitle(bquote("Side by Side Boxplots of Rating Between Categories")) +
        labs(subtitle = "Data obtained from Google Play Store",
             x = "Category",
             y = "Rating") +
        theme_bw() +
        theme(plot.title = element_text(face = "bold"),
              plot.subtitle = element_text(color = "grey"),
              axis.text.x = element_text(angle = 90, vjust = 0.5))
}

# Call the function
Rating_Category(googleclean_df)
