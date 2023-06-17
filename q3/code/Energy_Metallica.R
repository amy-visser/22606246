library(ggridges)

Energy_Metallica <- function(metallica_df) {
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


    ggplot(filtered_df, aes(x = energy, y = fct_rev(album), fill = album)) +
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
                '72 Seasons - 2023',
                'S&M2 - 2021',
                'Hardwired to Self-Destruct - 2016',
                'Lulu - 2011',
                'Six Feet Down Under - 2010',
                'Six feet Down Under Part 2 - 2010',
                'Death Magnetic - 2008',
                'Some Kind of Monster - 2004',
                'St. Anger - 2003',
                'S&M - 1999',
                'Garage, Inc. - 1998',
                'Reload - 1997',
                'Load - 1996',
                'Metallica - 1991',
                '...And Justice for All - 1988',
                'Master of Puppets (Remastered) - 1986',
                'Ride the Lightning (Remastered) - 1984',
                'Kill em All (Remastered) - 1983'
            )
        ) +
        scale_x_continuous(expand = c(0, 0)) +
        labs(
            title = "Metallica Energy by Album",
            subtitle = "Based on data from Spotify",
            y = "Album",
            x = "Energy"
        ) +
        guides(fill = FALSE)
}