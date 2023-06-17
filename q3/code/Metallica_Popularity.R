Metallica_Popularity <- function(metallica_df) {
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
        filter(!release_date %in% c('2020/04/15', '2020/04/18', '2020/04/21')) %>%
        arrange(as.Date(release_date, format = "%Y/%m/%d"))  # Arrange by release_date column

    ggplot(filtered_df, aes(x = album, y = popularity, fill = album)) +
        geom_boxplot(width = 0.7) +
        labs(x = "Album", y = "Popularity", title = "Metallica Popularity by Album") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
              legend.position = "none")
}