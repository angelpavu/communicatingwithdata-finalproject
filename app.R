#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(scales)

#removes listings with 0 price and rows where the borough is missing
df <- read_csv("AB_NYC_2019.csv", show_col_types = FALSE) %>%
  filter(price > 0, !is.na(neighbourhood_group))
#summary columns for each borough showing median price, mean price, how many listings in each
borough_stats <- df %>%
  group_by(neighbourhood_group) %>%
  summarise(
    median_price = median(price, na.rm = TRUE),
    mean_price = mean(price, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(median_price))
#now summarizing up by the neighborhoods in each borough, so we can see the top neighborhoods
#making sure to only include neighborhoods with at least 50 listings as to not skew the median, so analysis is more reliable
neighborhood_stats <- df %>%
  group_by(neighbourhood, neighbourhood_group) %>%
  summarise(
    median_price = median(price, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 50) %>%
  arrange(desc(median_price))

#a chart showing price ranges for room types using percentiles
room_stats <- df %>%
  filter(price <= 600) %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(
    p10 = quantile(price, 0.10, na.rm = TRUE),
    p25 = quantile(price, 0.25, na.rm = TRUE),
    median = median(price, na.rm = TRUE),
    p75 = quantile(price, 0.75, na.rm = TRUE),
    p90 = quantile(price, 0.90, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

#puts each listing into a review category based on the number of reviews it has
#also calculates the median price and number of listings for each review group
#answers our question of: do listings with more reviews cost more?
review_stats <- df %>%
  mutate(
    review_group = cut(
      number_of_reviews,
      breaks = c(-1, 0, 5, 20, 50, 100, Inf),
      labels = c("0", "1-5", "6-20", "21-50", "51-100", "100+")
    )
  ) %>%
  group_by(review_group) %>%
  summarise(
    median_price = median(price, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  drop_na()


ui <- fluidPage(

   
)


server <- function(input, output) {

   
}

# Run the application 
shinyApp(ui = ui, server = server)
