App_Install <- function(googleclean_df) {

    ggplot(googleclean_df, aes(x = Category, y = Installs)) +
        geom_bar(stat = "identity", width = 0.8, fill = "#FF69B4", colour = "white") +
        coord_flip() +
        labs(
            title = "Total App Installation for Each Category",
            subtitle = "Data sourced from Google Play"
        ) +
        theme(
            axis.text.x = element_text(angle = 90),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12, face = "italic")
        )

}
