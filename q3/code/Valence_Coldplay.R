Valence_Coldplay <- function(coldplay_df) {
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
        ))

    filtered_df %>% ggplot(aes(x = valence, y = album_name, fill = ..x..)) +
        geom_density_ridges_gradient(scale = 0.9) +
        scale_fill_gradient(low = "grey", high = "green4") +
        theme_bw() +
        theme(panel.background = element_rect(fill = "white")) +
        theme(plot.background = element_rect(fill = "white")) +
        xlim(0,1) +
        theme(legend.position = "none")
}
