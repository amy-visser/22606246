library(ggplot2)
library(dplyr)
library(ggrepel)

Coldplay_Popularity <- function(coldplay_df) {
    filtered_df <- coldplay_df %>%
        filter(!album_name %in% c(
            'Live in Buenos Aires',
            'Love in Tokyo',
            'A Head Full of Dreams Tour Edition',
            'Ghost Stories Live 2014',
            'Live 2012',
            'LeftRightLeftRightLeft (Live)',
            'Viva La Vida (Prospekts March Edition)',
            'Live 2003'
        )) %>%
        arrange(as.Date(release_date, format = "%Y/%m/%d"))  # Arrange by release_date column

    ggplot(filtered_df, aes(x = album_name, y = popularity, fill = album_name)) +
        geom_boxplot(width = 0.7) +
        labs(x = "Album", y = "Popularity", title = "Coldplay Popularity by Album") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
              legend.position = "none")
}

