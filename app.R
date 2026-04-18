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

#here, we have bunched up a couple of calculations that we want to highlight within the data story
#these will be easy access when we want to bring attention to key headers/titles
total_listings <- nrow(df)

manhattan_median <- borough_stats %>%
  filter(neighbourhood_group == "Manhattan") %>%
  pull(median_price)

bronx_median <- borough_stats %>%
  filter(neighbourhood_group == "Bronx") %>%
  pull(median_price)

room_medians <- df %>%
  group_by(room_type) %>%
  summarise(median_price = median(price, na.rm = TRUE), .groups = "drop")

entirehome_median <- room_medians %>%
  filter(room_type == "Entire home/apt") %>%
  pull(median_price)

privateroom_median <- room_medians %>%
  filter(room_type == "Private room") %>%
  pull(median_price)

sharedroom_median <- room_medians %>%
  filter(room_type == "Shared room") %>%
  pull(median_price)

review_correlation <- cor(df$number_of_reviews, df$price, use = "complete.obs")

# used https://coolors.co/ for help with deciding color schemes, along with getting all the hex codes
boro_cols <- c(
  "Manhattan"     = "#b23a2f",
  "Brooklyn"      = "#d17a22",
  "Queens"        = "#2b6ea6",
  "Staten Island" = "#7a4da3",
  "Bronx"         = "#2f7d4a"
)

room_cols <- c(
  "Entire home/apt" = "red",
  "Private room"    = "blue",
  "Shared room"     = "green"
)

# we created a central story theme, which we hope mimics the style of NYT. 
#also gained some inspiration from https://flourish.studio/examples/ 
story_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "#f7f4ee", colour = NA),
      panel.background = element_rect(fill = "#f7f4ee", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#ddd6ca", linewidth = 0.45),
      plot.title = element_text(face = "bold", size = 15, colour = "#1f1f1f"),
      plot.subtitle = element_text(size = 11, colour = "#4f4f4f"),
      plot.caption = element_text(size = 9, colour = "#777777"),
      axis.title = element_text(size = 10, colour = "#666666"),
      axis.text = element_text(size = 11, colour = "#333333"),
      legend.position = "top",
      legend.title = element_blank()
    )
}

## used https://shiny.posit.co/r/articles/build/ui/ and https://getbootstrap.com/docs/5.0/layout/containers/ for help on the overall structure and syntax
##We used tags$head and tags$style to add CSS styling to the Shiny app so we could control the colors, layout, and fonts beyond the default stuff.
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: white;
        color: #1d1d1d;
        font-family: Georgia, 'Times New Roman', serif;
      }

      .topbar {
        background: #1d1d1d;
        color: #f4f1ea;
        text-align: center;
        padding: 14px 10px;
        border-bottom: 3px solid #3b82f6;
        letter-spacing: 1px;
        font-size: 12px;
        text-transform: uppercase;
      }

      .hero {
        background: #1d1d1d;
        color: #f4f1ea;
        padding: 70px 25px 55px 25px;
        text-align: center;
      }

      .hero-small {
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 2px;
        color: #3b82f6;
        margin-bottom: 18px;
      }

      .hero-title {
        font-size: 56px;
        font-weight: bold;
        line-height: 1.1;
        margin-bottom: 20px;
      }

      .hero-title em {
        color: #3b82f6;
        font-style: italic;
      }

      .hero-deck {
        max-width: 700px;
        margin: 0 auto;
        font-size: 21px;
        line-height: 1.6;
        color: #e8e1d5;
      }

      .article {
        max-width: 860px;
        margin: 50px auto 0 auto;
        padding: 0 24px 50px 24px;
      }

      .section-rule {
        text-align: center;
        margin: 55px 0 30px 0;
        color: #8a7d6b;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 2px;
      }

      .chapter-tag {
        color: #3b82f6;
        font-size: 13px;
        text-transform: uppercase;
        letter-spacing: 2px;
        margin-bottom: 10px;
        font-weight: bold;
      }

      .story-h2 {
        font-size: 34px;
        font-weight: bold;
        line-height: 1.2;
        margin-bottom: 22px;
        color: #1d1d1d;
      }

      .story-h2 em {
        color: #3b82f6;
        font-style: italic;
      }

      .dropcap::first-letter {
        float: left;
        font-size: 4.5em;
        line-height: 0.85;
        padding-right: 8px;
        font-weight: bold;
      }

      .story-p {
        font-size: 19px;
        line-height: 1.85;
        margin-bottom: 1.3em;
      }

      .stat-row {
        display: flex;
        gap: 12px;
        margin: 38px 0;
        flex-wrap: wrap;
      }

      .stat-box {
        flex: 1;
        min-width: 220px;
        background: #fbf9f4;
        border: 1px solid #ddd6ca;
        padding: 24px 18px;
        text-align: center;
      }

      .stat-num {
        font-size: 38px;
        font-weight: bold;
        color: #3b82f6;
        display: block;
        margin-bottom: 6px;
      }

      .stat-label {
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 1.5px;
        color: #7c7165;
      }

      .viz-block {
        background: #efe8dd;
        border: 1px solid #ddd6ca;
        padding: 24px 20px 14px 20px;
        margin: 36px 0;
      }

      .viz-title {
        font-size: 20px;
        font-weight: bold;
        margin-bottom: 4px;
      }

      .viz-sub {
        font-size: 14px;
        line-height: 1.6;
        color: #4d453d;
        margin-bottom: 14px;
      }

      .pullquote {
        border-left: 4px solid #3b82f6;
        background: #efe8dd;
        padding: 18px 22px;
        margin: 35px 0;
      }

      .pullquote p {
        font-size: 25px;
        line-height: 1.5;
        font-style: italic;
        margin: 0 0 10px 0;
      }

      .pullquote cite {
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 1.5px;
        color: #7c7165;
        font-style: normal;
      }

      .conclusion {
        background: #1d1d1d;
        color: #f4f1ea;
        padding: 55px 28px;
        text-align: center;
        margin-top: 60px;
      }

      .conclusion h2 {
        font-size: 34px;
        margin-bottom: 20px;
      }

      .conclusion p {
        max-width: 720px;
        margin: 0 auto 18px auto;
        font-size: 18px;
        line-height: 1.8;
        color: #e7dfd2;
      }

      .conclusion .kicker {
        color: #3b82f6;
        font-style: italic;
        font-size: 22px;
      }

      .footer {
        background: #1d1d1d;
        color: #a79c8d;
        text-align: center;
        padding: 20px;
        font-size: 11px;
        letter-spacing: 1px;
        text-transform: uppercase;
      }
    "))
  ),
  
  div(class = "topbar", "NYC Airbnb Open Data · Kaggle 2019"),
  
  div(
    class = "hero",
    div(class = "hero-small", "48,895 listings · 5 boroughs · 1 question"),
    div(class = "hero-title", HTML("What Are You <em>Truly</em> Paying For?")),
    div(
      class = "hero-deck",
      "A group of friends open Airbnb and look at places to stay in New York City. The prices range from $45 to $800 a night.
       The listings look very similar. So how come the pricing is so different?"
    )
  ),
  
  div(
    class = "article",
    
    
    p(
      class = "story-p dropcap",
      "Imagine you open Airbnb looking for a place to stay for a weekend in New York City. After scouring through
      the website, you narrow it down to two choices. Both are studio apartmets. Both have solid reviews.
       Both claim to be cozy and well-located. One costs $100 a night. The other one costs $350.
       You wonder what could explain the $250 difference, and if there is something you are not seeing. Don't worry, you are not
       missing anything."
    ),
    
    p(
      class = "story-p",
      "We analyzed all 48,895 Airbnb listings across New York City's five boroughs to find
       out what actually drives the price of these listings. We looked at geography, room type, and review counts.
       What we found confirms some of our previous expectations, but also disproves some others."
    ),
    
    div(
      class = "stat-row",
      div(
        class = "stat-box",
        span(class = "stat-num", "48,895"),
        span(class = "stat-label", "Real Listings Analyzed")
      ),
      div(
        class = "stat-box",
        span(class = "stat-num", paste0("$", round(manhattan_median))),
        span(class = "stat-label", "Manhattan Median Per Night")
      ),
      div(
        class = "stat-box",
        span(class = "stat-num", paste0(round(entirehome_median / privateroom_median, 1), "x")),
        span(class = "stat-label", "Entire Home vs. Private Room")
      )
    ),
    
    div(class = "section-rule", "Chapter I - Location"),
    div(class = "chapter-tag", "Part One"),
    div(class = "story-h2", HTML("The Borough Gap Is Real, However <em>Neighborhoods Are What Scramble the Map</em>")),
    
    p(
      class = "story-p",
      "The most expected finding comes first: where you stay matters enormously. Manhattan's
       median listing sits at $150 per night, which is more than double the Bronx's $65. But that
       borough-level spread is only the beginning. When you zoom into individual neighborhoods,
       the picture becomes far more layered."
    ),
    
    p(
      class = "story-p",
      "Tribeca is the most expensive-stay neighborhood that tops all of NYC with a median of $295 a night. Flatiron District hits $225.
       But the more telling story is what is happening across the river: DUMBO (Down Under the Manhattan Bridge Overpass) in Brooklyn
       reaches $189. This is higher than many neighborhoods tourists consider squarely Manhattan.
       The geographic hierarchy is not a simple gradient from center to edge. It is a patchwork
       shaped by prestige, proximity, and the slow march of gentrification."
    ),
    
    div(
      class = "viz-block",
      div(class = "viz-title", "NYC Airbnb Prices by Borough and Neighborhood"),
      div(
        class = "viz-sub",
        "Borough medians shown as bars. Top neighborhood medians (50+ listings) overlaid as points."
      ),
      plotOutput("plot_location", height = "430px")
    ),
    
    p(
      class = "story-p",
      "The implication for travelers is clear but counterintuitive: booking Brooklyn to save
       money is a gamble. The borough you are in matters less than the
       specific pocket of the city you have landed in."
    ),
    
    div(
      class = "pullquote",
      tags$p(
        "DUMBO in Brooklyn commands a higher median Airbnb price than much of Harlem.
         The borough label is a starting point, not a price tag."
      ),
      tags$cite("Finding #1 · Price by Geography")
    ),
    
    div(class = "section-rule", "Chapter II - Room Type"),
    div(class = "chapter-tag", "Part Two"),
    div(class = "story-h2", HTML("You Are Not Paying for Quality. <em>You Are Paying for a Closed Door.</em>")),
    
    p(
      class = "story-p",
      "Location sets the baseline, but it is ultimately room type which determines the tier, and the data here is
       very clear. Entire home listings in NYC have a median of $160 per night.
       Private rooms come in at $70. Shared rooms which give way to less privacy average $45. These are not subtle differences, but rather three entirely
       separate markets operating under one brand name."
    ),
    
    p(
      class = "story-p",
      "But the more revealing story emerges when you look at the full distributions, not
       just the medians. A Manhattan entire-home listing at the 90th percentile costs $399
       a night. A Manhattan private room at the 90th percentile costs $175. The best private
       room in the city is still cheaper than a middle-of-the-road entire apartment. The
       ceiling of one tier barely reaches the floor of the next."
    ),
    
    div(
      class = "viz-block",
      div(class = "viz-title", "The Price of Privacy: Full Price Ranges by Room Type and Borough"),
      div(
        class = "viz-sub",
        "Line = 10th to 90th percentile. Point = median."
      ),
      plotOutput("plot_roomtype", height = "420px")
    ),
    
    p(
      class = "story-p",
      "From the plot, it is evident you are not booking a discounted version of the same
       experience when you choose a private room. You are booking a fundamentally different
       product. One that costs less not because of quality, but because of the single
       feature that separates them: a door that closes."
    ),
    
    div(class = "section-rule", "Chapter III - Reviews"),
    div(class = "chapter-tag", "Part Three"),
    div(class = "story-h2", HTML("The Most Counterintuitive Finding: <em>More Reviews, Slightly Lower Price</em>")),
    
    p(
      class = "story-p",
      "Here is where the data may surprise you. Most travelers assume that a listing
       with 100 five-star reviews warrants a higher price than one with 15. That the wisdom of
       crowds has been taken account into the rate. That hosts who have earned their reputation charge
       at a higher rate."
    ),
    
    p(
      class = "story-p",
      "They do not. In fact, the relationship almost runs in the opposite direction. Across all 48,895
       NYC listings, the Pearson correlation between review count and price is negative, being -0.05. Listings
       with zero reviews have a median price of $120. Listings with more than 100 reviews sit
       at $103. More reviews, lower price, a finding that holds even after controlling for
       borough and room type."
    ),
    
    div(
      class = "viz-block",
      div(class = "viz-title", "Do More Reviews Mean Higher Prices? The Data Says No."),
      div(
        class = "viz-sub",
        "Median nightly price by review bracket."
      ),
      plotOutput("plot_reviews", height = "330px")
    ),
    
    p(
      class = "story-p",
      "The explanation, once you see it, makes sense: hosts price their listings based on
       what the market will bear for their location and space, and not on the reputation they
       have built. A host with 300 glowing reviews in Mott Haven still cannot charge Tribeca
       prices. A brand-new listing with zero reviews in the West Village can charge whatever
       the neighborhood supports. The market is pricing the address. Not the host."
    ),
    
    div(
      class = "conclusion",
      h2(HTML("So What Are You <em>Actually</em> Paying For?")),
      p(
        "Three things. Location, but specifically the neighborhood, not just the borough. This sets
         the baseline. Room type determines your tier, and those tiers barely overlap.
         And reputation? It registers at essentially zero in the price equation."
      ),
      p(
        "This should change how you shop. Stop reading review counts as price signals. Stop
         assuming a well-reviewed listing justifies its rate. Instead, ask two questions
         before anything else: Exactly where is it and how much privacy am I getting?"
      ),
      p(
        "The price of a New York Airbnb turns out to be strikingly literal. You pay for a
         zip code. You pay for privacy. The charm, the host's story, the glowing reviews are
         all real. They just do not show up in the number."
      ),
      p(
        class = "kicker",
        "In New York's Airbnb market, space and location are everything. Reputation costs nothing."
      )
    )
  ),
  
  div(
    class = "footer",
    "Data: NYC Airbnb Open Data · Kaggle, 2019"
  )
   
)


server <- function(input, output) {

   
}

# Run the application 
shinyApp(ui = ui, server = server)
