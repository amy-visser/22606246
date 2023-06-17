library(ggridges)

Energy_Coldplay <- function(coldplay_df) {
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

    ggplot(filtered_df, aes(x = energy, y = fct_rev(album_name), fill = album_name)) +
        geom_density_ridges(
            scale = 2,
            size = 0.1,
            rel_min_height = 0.03,
            quantile_lines = TRUE,
            quantiles = 2
        ) +
        theme_ridges() +
        scale_y_discrete(
            labels = c(
                'Music of the Spheres - 2021',
                'Everyday Life - 2019',
                'A Head Full of Dreams - 2015',
                'Ghost Stories - 2014',
                'Mylo Xyloto - 2011',
                'Viva La Vida or Death and All His Friends - 2008',
                'X&Y - 2005',
                'A Rush of Blood to the Head - 2002',
                'Parachutes - 2000'
            )
        ) +
        scale_x_continuous(expand = c(0, 0)) +
        labs(
            title = "Coldplay Energy by Album",
            subtitle = "Based on data from Spotify",
            y = "Album",
            x = "Energy"
        ) +
        guides(fill = FALSE)
}
