IMDB <- function(netflix_df){
#
# df_plot <-
#     netflix_df %>%
#     mutate(Text = ifelse(imdb_score == min(imdb_score, na.rm=T), glue::glue("Worst:  ({imdb_score})\n{title}"),
#                          ifelse(imdb_score == max(imdb_score, na.rm=T), glue::glue("Best: ({imdb_score})\n{title}"),
#                                 NA_character_)))
#
#
# df_plot %>%
#     ggplot() +
#     geom_boxplot(aes(x = title, y = imdb_score, fill = title), alpha = 0.4) +
#     geom_jitter(aes(title, imdb_score, color = title), size = 3, alpha = 0.8) +
#     scale_y_continuous(limits = c(15, 60), breaks = scales::breaks_pretty(n = 10)) +
#     ggrepel::geom_text_repel(aes(x = title, y = imdb_score, label = Text),
#                              force = T ) +
#     theme_bw() +
#     labs(x = "Title", y = "IMDB Score", caption = "Calculated using the Netflix and IMDB database",
#          title = "IMDB Score per Offering") +
#     scale_fill_hue(l=40, c=35) +
#     scale_color_hue(l=40, c=35) +
#     guides(fill = F, color = F)

    library(ggplot2)
    library(dplyr)
    library(glue)

    df_plot <- netflix_df %>%
        mutate(Text = ifelse(imdb_score == min(imdb_score, na.rm = TRUE), glue("Worst: ({imdb_score})\n{title}"),
                             ifelse(imdb_score == max(imdb_score, na.rm = TRUE), glue("Best: ({imdb_score})\n{title}"),
                                    NA_character_)))

    ggplot(df_plot, aes(x = title, y = imdb_score)) +
        geom_text(aes(label = Text), vjust = -0.5, hjust = 0, size = 3, color = "black") +
        coord_flip() +
        labs(x = "Title", y = "IMDB Score") +
        theme_bw()

}