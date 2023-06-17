App_Rating <- function(googleclean_df){

    ggplot(googleclean_df, aes(x = Rating)) +
        geom_histogram(fill = "palegreen3", color = "white") +
        labs(title = "Distribution of App Ratings",
             subtitle = "Histogram of ratings for the selected apps",
             x = "Rating",
             y = "Count") +
        theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
              plot.subtitle = element_text(color = "grey", size = 14))

}
