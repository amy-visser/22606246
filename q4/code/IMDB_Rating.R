IMDB_Rating <- function(netflix_df) {

head(arrange(netflix_df[,c("title", "imdb_votes", "imdb_score")],desc(imdb_votes)),10)

}