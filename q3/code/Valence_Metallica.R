Valence_Metallica <- function(metallica_df) {
    filtered_df <- metallica_df %>%
        filter(!album %in% c(
            'Metallica (Remastered Deluxe Box Set)',
            'Metallica (Remastered 2021)',
            'Helping HandsLive & Acoustic At The Masonic',
            'Helping Hands...Live & Acoustic at The Masonic',
            'HardwiredTo Self-Destruct (Deluxe)',
            'Metallica Through The Never (Music From The Motion Picture)',
            'Some Kind Of Monster (Live)',
            'Live S**t: Binge & Purge',
            'Live Sh*t: Binge & Purge (Live In Mexico City)',
            'Metallica (Remastered)',
            '...And Justice for All (Remastered Deluxe Box Set)',
            'And Justice for All (Remastered Deluxe Box Set)',
            'And Justice for All (Remastered)',
            'And Justice for All (Remastered Deluxe Box Set)',
            '...And Justice for All (Remastered)',
            'Master Of Puppets (Deluxe Box Set / Remastered)',
            'Master of Puppets (Remastered Deluxe Box Set)',
            'Ride The Lightning (Deluxe Remaster)',
            'Ride The Lightning (Deluxe / Remastered)',
            'Kill Em All (Deluxe Remaster)',
            'Kill Em All (Deluxe / Remastered)',
            'Garage Inc.'
        )) %>%
        filter(!release_date %in% c('2020/04/15', '2020/04/18', '2020/04/21'))

    filtered_df %>% ggplot(aes(x = valence, y = album, fill = ..x..)) +
        geom_density_ridges_gradient(scale = 0.9) +
        scale_fill_gradient(low = "grey", high = "red2") +
        theme_bw() +
        theme(panel.background = element_rect(fill = "white")) +
        theme(plot.background = element_rect(fill = "white")) +
        xlim(0,1) +
        theme(legend.position = "none")
}

