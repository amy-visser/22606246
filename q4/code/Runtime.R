
Runtime <- function(netflix_df){

datasummary((`Type` = type)*(`Duration` = runtime) ~ Min + Max + Mean + Median + N , data = netflix_df, title = "Runtime Summary")

}