library(ggplot2)
library(dplyr)

BarGraph_FreePaid <- function(googleclean_df) {
    df_summary <- googleclean_df %>%
        group_by(Category, Type) %>%
        summarise(App_Count = n()) %>%
        pivot_wider(names_from = Type, values_from = App_Count, values_fill = 0)

    colors <- c("#66C2A5", "#FC8D62")  # Specify different colors for Free and Paid

    ggplot(df_summary, aes(x = Category)) +
        geom_bar(aes(y = Free, fill = "Free"), stat = "identity") +
        geom_bar(aes(y = Paid, fill = "Paid"), stat = "identity") +
        scale_fill_manual(values = colors, guide = guide_legend(title = "Type")) +
        labs(title = "Number of Free Apps vs Paid Apps per Category",
             x = "Category",
             y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

BarGraph_FreePaid(googleclean_df)
