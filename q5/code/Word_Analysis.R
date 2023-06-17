Word_Analysis <- function(googlereview_df) {
    library(wordcloud2)
    library(RColorBrewer)

    # Calculate word frequencies
    word_freq <- table(googlereview_df$Translated_Review)

    # Convert word frequencies to data frame
    word_data <- data.frame(word = names(word_freq), freq = as.numeric(word_freq))

    # Generate word cloud
    wordcloud2(word_data, size = 0.5, minSize = 10, color = brewer.pal(8, "Dark2"),
               backgroundColor = "white")
}
