library(modelsummary)

Show_or_Movie <- function(netflix_df){
datasummary((` Type` = type) ~ N + Percent(), data = netflix_df, title = "Netflix Content Type")
}