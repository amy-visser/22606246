library(tidyverse)
library(ggplot2)

Genre_and_Rating <- function(netflix_df) {
    # Extract one genre per film
    movies <- netflix_df %>%
        filter(type == "MOVIE") %>%
        separate_rows(genres, sep = ",") %>%
        mutate(genres = trimws(genres)) %>%
        distinct(title, .keep_all = TRUE) %>%
        mutate(genres = factor(genres))

    ggplot(movies, aes(x = genres, y = tmdb_score)) +
        geom_boxplot() +
        coord_flip() +
        labs(title = "Distribution of TMDB Score by Genre for Movies",
             x = "Genre",
             y = "TMDB Score") +
        theme_bw()
}
