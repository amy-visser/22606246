Show_Seasons <- function(netflix_df) {
    ggplot(data = netflix_df[netflix_df$type == "SHOW", ], aes(x = seasons)) +
        geom_bar(fill = "red3", alpha = 0.8) +
        xlab("Number of Seasons") +
        ylab("Count") +
        labs(title = "Netflix TV Shows Seasons Distribution") +
        theme(legend.position = "top",
              panel.border = element_blank(), axis.text = element_text(size = 8),
              plot.title = element_text(size = 12L, face = "bold", hjust = 0.5),
              panel.background = element_rect(fill = NA)) +
        coord_cartesian(xlim = c(0, 15))
}

