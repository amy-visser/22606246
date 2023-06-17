library(ggplot2)
library(dplyr)

Year_and_Rating <- function(netflix_df) {
    movies <- netflix_df %>%
        filter(type == "MOVIE") %>%
        mutate(release_year = factor(release_year))

    ggplot(movies, aes(x = runtime, y = imdb_score)) +
        geom_point() +
        labs(x = "Runtime", y = "IMDB Score", title = "A Scatterplot Detailing the Movie Offerings on Netflix") +
        theme_bw()

    movies <- mutate(movies, release_year = factor(release_year))

    ggplot(movies, aes(x = runtime, y = imdb_score, color = release_year)) +
        geom_point() +
        labs(x = "Minutes", y = "Rating", title = "A Scatterplot Detailing the Movie Offerings on Netflix") +
        theme_bw()
}
