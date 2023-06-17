library(modelsummary)

Age_Certification <- function(netflix_df) {
    age_cert_levels <- levels(factor(age_certification))
    datasummary((`Age Certification` = factor(netflix_df$age_certification, levels = age_cert_levels)) ~ N + Percent(), data = netflix_df, title = "Age Certification Categories")
}
