library(shiny)
library(tidyverse)
library(scales)
library(leaflet)
library(plotly)

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

room_medians <- df %>%
  group_by(room_type) %>%
  summarise(median_price = median(price, na.rm = TRUE), .groups = "drop")

entirehome_median <- room_medians %>%
  filter(room_type == "Entire home/apt") %>%
  pull(median_price)

privateroom_median <- room_medians %>%
  filter(room_type == "Private Room") %>%
  pull(median_price)

sharedroom_median <- room_medians %>%
  filter(room_type == "Shared Room") %>%
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
  "Entire home/apt" = "darkred",
  "Private room"    = "darkblue",
  "Shared room"     = "darkgreen"
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

## used https://shiny.posit.co/r/articles/build/ui/ and https://getbootstrap.com/docs/5.0/layout/containers/ for help on the overall structure and syntax.
## used https://shiny.posit.co/r/articles/build/css/ for help getting started on integrating CSS/HTML
##We used tags$head and tags$style to add CSS styling to the Shiny app so we could control the colors, layout, and fonts beyond the default stuff.
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: white;
        color: #1d1d1d;
        font-family: Georgia, 'Times New Roman', serif;
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
      
      .explore-section {
        background: #1d1d1d;
        color: #f4f1ea;
        text-align: center;
        padding: 70px 25px;
        margin-top: 60px;
      }

      .explore-small {
        font-size: 12px;
        letter-spacing: 2px;
        text-transform: uppercase;
        color: #3b82f6;
        margin-bottom: 15px;
      }

      .explore-title {
        font-size: 38px;
        font-weight: bold;
        margin-bottom: 20px;
      }

      .explore-text {
        max-width: 650px;
        margin: 0 auto 25px auto;
        font-size: 18px;
        line-height: 1.6;
        color: #e7dfd2;
      }

      .explore-btn {
        display: inline-block;
        background: #3b82f6;
        color: white !important;
        padding: 14px 28px;
        border-radius: 8px;
        text-decoration: none;
        font-size: 16px;
        font-weight: bold;
      }

      .explore-btn:hover {
        background: #2563eb;
      }
      
      .interactive-section {
        max-width: 1000px;
        margin: 60px auto 40px auto;
        padding: 0 24px;
      }

      .nav-tabs {
        margin-bottom: 20px;
      }

      .nav-tabs > li > a {
        color: #1d1d1d;
        font-weight: bold;
      }

      .tab-content {
        background: white;
        border: 1px solid #ddd;
        border-top: none;
        padding: 20px;
      }

      .sidebar-box {
        background: #f7f4ee;
        border: 1px solid #ddd6ca;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
      }
      
      .builder-results {
        display: flex;
        gap: 20px;
        flex-wrap: wrap;
        margin-top: 20px;
      }

      .builder-card {
        flex: 1;
        min-width: 240px;
        background: white;
        border: 1px solid #d9d9d9;
        padding: 28px 24px;
        text-align: center;
        border-radius: 8px;
      }

      .builder-label {
        font-size: 13px;
        text-transform: uppercase;
        letter-spacing: 1px;
        color: #666666;
        margin-bottom: 10px;
      }

      .builder-value {
        font-size: 42px;
        font-weight: bold;
        color: #3b82f6;
        line-height: 1.1;
      }

      .builder-subtext {
        margin-top: 18px;
        font-size: 16px;
        line-height: 1.6;
        color: #444444;
      }
    "))
  ),
  
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
      the website, you narrow it down to two choices. Both are studio apartments. Both have solid reviews.
       Both claim to be cozy and well-located. One costs $100 a night. The other one costs $350.
       You wonder what could explain the $250 difference, and if there is something you are not seeing. Don't worry, you are not
       missing anything."
    ),
    
    p(
      class = "story-p",
      "We analyzed all 48,895 Airbnb listings across New York City's five boroughs to find
       out what actually drives the price of these listings. We looked at geography, room type, and review counts.
       What we found confirms some of our previous expectations, but also gave us some surprises."
    ),
    
    div(class = "chapter-tag", "Chapter I-Location"),
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
      "Tribeca is the most expensive-stay neighborhood that tops all of NYC with a median of $295 a night, and Flatiron District is not far behind at $225.
       But the true story is that New York does not price Airbnb listings in neat borough-wide tiers. Travelers may assume that leaving Manhattan guarantees a bargain, yet the data shows a more uneven reality.
      Expensive pockets appear across NYC, and cheaper ones do too. What looks like a borough gap is really a neighborhood mosaic."
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
      "The implication for travelers is clear but counterintuitive: booking Brooklyn instead of Manhattan to save
       money is a gamble. The borough you are in matters less than the
       specific pocket of the city you have landed in."
    ),
    
    div(
      class = "pullquote",
      tags$p(
        "The dots representing the top neighborhood medians are proof that expensive listings are found in each borough.
         The borough label is a starting point, not a price tag, and the neighborhood is really what seals the deal."
      ),
      tags$cite("Finding #1 · Price by Geography")
    ),
    
    div(class = "chapter-tag", "Chapter II - Room Type"),
    div(class = "story-h2", HTML("You Are Not Paying for Quality. <em>You Are Paying for Space and Privacy.</em>")),
    
    p(
      class = "story-p",
      "Location sets the baseline, but it is ultimately room type which determines the tier, and the data here is
       very clear. Entire home listings in NYC have a median of $160 per night.
       Private rooms come in at $70. Shared rooms which give way to less privacy average $45. These are not subtle differences, but rather three entirely
       separate markets operating under one brand name."
    ),
    
    p(
      class = "story-p",
      "However the hidden story emerges when you look at the full distributions, not
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
        "Line = middle 50% range (25th to 75th percentile). Point = median."
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
    
    div(class = "chapter-tag", "Chapter III - Reviews"),
    div(class = "story-h2", HTML("The Most Counterintuitive Finding: <em>More Reviews, Slightly Lower Price</em>")),
    
    p(
      class = "story-p",
      "Here is where the data may surprise you. Most travelers assume that a listing
       with 100 five-star reviews warrants a higher price than one with 15. That the opinion of the people
       has been taken account into the rate. That hosts who have earned their reputation charge
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
      div(class = "viz-title", "Do More Reviews Lead To Higher Listing Prices? The Data Says No."),
      div(
        class = "viz-sub",
        "Median nightly price by review bracket."
      ),
      plotOutput("plot_reviews", height = "330px")
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
        "The price of a New York Airbnb turns out to be strikingly literal. You pay for a
         zip code. You pay for privacy."
      ),
      p(
        class = "kicker",
        "In New York's Airbnb market, space and location are everything. Reputation costs nothing."
      )
    )
  ),
  
  div(
    class = "explore-section",
    div(class = "explore-small", "Now It Is Your Turn"),
    div(class = "explore-title", HTML("Explore the <em>New York Airbnb Market</em>")),
    tags$a(href = "#interactive-tools", class = "explore-btn", "Start Exploring")
  ),
  
  hr(),
  
  div(
    id = "interactive-tools",
    class = "interactive-section",
    h2("Interactive Visualizations"),
    
    tabsetPanel(
      id = "viz_tabs",
      
      tabPanel(
        "Interactive Map",
        br(),
        fluidRow(
          column(4, 
                 div(class = "sidebar-box",
                     sliderInput("map_price", "Price range", min = 0, max = 1000, value = c(50, 300), step = 10),
                     selectInput("map_room", "Room type", choices = c("All", sort(unique(df$room_type)))),
                     selectInput("map_borough", "Borough", choices = c("All", sort(unique(df$neighbourhood_group))))
                 )
          ),
          column(8, 
                 leafletOutput("listing_map", height = "500px")
          )
        )
      ),
      
      tabPanel(
        "Build Your Listing",
        br(),
        fluidRow(
          column(4,
                 div(class = "sidebar-box",
                     selectInput("build_borough", "Choose borough", choices = sort(unique(df$neighbourhood_group))),
                     uiOutput("build_neighborhood_ui"),
                     selectInput("build_room", "Choose room type", choices = sort(unique(df$room_type))),
                     sliderInput("build_min_nights", "Minimum nights", min = 1, max = 30, value = 1, step = 1),
                     sliderInput("build_reviews", "Minimum number of reviews", min = 0, max = 300, value = 0, step = 1)
                 )
          ),
          column(8,
                 div(class = "builder-results",
                     div(class = "builder-card",
                         div(class = "builder-label", "Estimated Nightly Price"),
                         div(class = "builder-value", textOutput("build_price"))
                     ),
                     div(class = "builder-card",
                         div(class = "builder-label", "Matching Listings"),
                         div(class = "builder-value", textOutput("build_matches"))
                     )
                 ),
                 div(class = "builder-subtext", textOutput("build_summary"))
          )
        )
      ),
      
      tabPanel(
        "Price Percentile Explorer",
        br(),
        fluidRow(
          column(4,
                 div(class = "sidebar-box",
                     selectInput("dist_borough", "Choose borough", choices = sort(unique(df$neighbourhood_group)), selected = "Manhattan"),
                     selectInput("dist_room", "Choose room type", choices = sort(unique(df$room_type)), selected = "Entire home/apt"),
                     sliderInput("dist_budget", "Choose a nightly budget", min = 0, max = 1000, value = 200, step = 10)
                 ),
                 h4(textOutput("dist_percentile")),
                 p(textOutput("dist_summary"))
          ),
          column(8,
                 plotOutput("price_distribution", height = "450px")
          )
        )
      ),
      
      tabPanel(
        "Availability Heatmap",
        br(),
        fluidRow(
          column(4,
                 div(class = "sidebar-box",
                     selectInput("heat_room", "Room type", choices = c("All", sort(unique(df$room_type))), selected = "All"),
                     sliderInput("heat_min_nights", "Max minimum nights", min = 1, max = 30, value = 30, step = 1),
                     sliderInput("heat_price", "Max price", min = 0, max = 1000, value = 500, step = 50)
                 )
          ),
          column(8,
                 plotlyOutput("heatmap_plot", height = "360px")
          )
        )
      )
    )
  ),
  
  div(class = "footer", "Data: NYC Airbnb Open Data · Kaggle, 2019")
)

server <- function(input, output) {
  output$plot_location <- renderPlot({
    boro_order <- borough_stats %>% arrange(median_price) %>% pull(neighbourhood_group)
    bs <- borough_stats %>% mutate(neighbourhood_group = factor(neighbourhood_group, levels = boro_order))
    ns <- neighborhood_stats %>% group_by(neighbourhood_group) %>% slice_max(order_by = median_price, n = 6, with_ties = FALSE) %>% ungroup() %>% mutate(neighbourhood_group = factor(neighbourhood_group, levels = boro_order))
    
    ggplot() +
      geom_col(data = bs, aes(x = median_price, y = neighbourhood_group, fill = neighbourhood_group), alpha = 0.15, width = 0.7) +
      geom_point(data = ns, aes(x = median_price, y = neighbourhood_group, colour = neighbourhood_group), size = 3.5, alpha = 0.8, position = position_jitter(height = 0.1, width = 0)) +
      geom_text(data = bs, aes(x = median_price, y = neighbourhood_group, label = paste0("$", round(median_price))), hjust = -0.2, size = 4, fontface = "bold", colour = "#111111") +
      scale_fill_manual(values = boro_cols, guide = "none") +
      scale_colour_manual(values = boro_cols, guide = "none") +
      scale_x_continuous(labels = dollar_format(), expand = expansion(mult = c(0, .15))) +
      labs(x = "Median nightly price (USD)", y = NULL) +
      story_theme()
  }, bg = "#f7f4ee")
  
  output$plot_roomtype <- renderPlot({
    room_order <- c("Bronx", "Staten Island", "Queens", "Brooklyn", "Manhattan")
    type_order <- c("Shared room", "Private room", "Entire home/apt")
    rs <- room_stats %>% mutate(neighbourhood_group = factor(neighbourhood_group, levels = room_order), room_type = factor(room_type, levels = type_order))
    
    ggplot(rs, aes(y = neighbourhood_group, colour = room_type)) +
      geom_segment(aes(x = p25, xend = p75, y = neighbourhood_group, yend = neighbourhood_group), linewidth = 4, alpha = 0.25, position = position_dodge(width = 0.7)) +
      geom_point(aes(x = median), size = 3.5, position = position_dodge(width = 0.7)) +
      scale_colour_manual(values = room_cols) +
      scale_x_continuous(labels = dollar_format(), expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Nightly price (USD)", y = NULL) +
      story_theme()
  }, bg = "#f7f4ee")
  
  output$plot_reviews <- renderPlot({
    ggplot(review_stats, aes(x = review_group, y = median_price)) +
      geom_col(fill = "#1d1d1d", alpha = 0.9, width = 0.6) +
      geom_text(aes(label = paste0("$", round(median_price))), vjust = -0.5, size = 4, fontface = "bold", colour = "#1d1d1d") +
      scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0, .2))) +
      labs(x = "Number of reviews", y = "Median nightly price") +
      story_theme()
  }, bg = "#f7f4ee")
  
  map_data <- reactive({
    filtered <- df %>% filter(price >= input$map_price[1], price <= input$map_price[2])
    if (input$map_room != "All") filtered <- filtered %>% filter(room_type == input$map_room)
    if (input$map_borough != "All") filtered <- filtered %>% filter(neighbourhood_group == input$map_borough)
    filtered <- filtered %>% filter(!is.na(latitude), !is.na(longitude))
    if (nrow(filtered) > 5000) filtered <- filtered %>% slice_sample(n = 5000)
    filtered
  })
  
  output$listing_map <- renderLeaflet({
    data <- map_data()
    pal <- colorNumeric(
      palette = c("#ebf8ff", "#3182ce", "#2c5282", "#1a365d"), 
      domain = data$price
    )
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude, radius = 4, stroke = FALSE, fillOpacity = 0.7, color = ~pal(price),
        label = ~paste0("Price: $", price, " | Neighborhood: ", neighbourhood)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~price, title = "Price")
  })
  
  output$build_neighborhood_ui <- renderUI({
    neighborhoods <- df %>% filter(neighbourhood_group == input$build_borough) %>% distinct(neighbourhood) %>% arrange(neighbourhood) %>% pull(neighbourhood)
    selectInput("build_neighborhood", "Choose neighborhood", choices = neighborhoods)
  })
  
  build_data <- reactive({
    req(input$build_neighborhood)
    df %>% filter(neighbourhood_group == input$build_borough, neighbourhood == input$build_neighborhood, room_type == input$build_room, minimum_nights >= input$build_min_nights, number_of_reviews >= input$build_reviews)
  })
  
  output$build_price <- renderText({
    data <- build_data()
    if (nrow(data) == 0) return("N/A")
    paste0("$", round(median(data$price, na.rm = TRUE)))
  })
  
  output$build_matches <- renderText({ paste0(nrow(build_data())) })
  
  dist_data <- reactive({ df %>% filter(neighbourhood_group == input$dist_borough, room_type == input$dist_room, price > 0, price <= 1000) })
  
  output$price_distribution <- renderPlot({
    data <- dist_data()
    req(nrow(data) > 0)
    med_price <- median(data$price, na.rm = TRUE)
    ggplot(data, aes(x = price)) +
      geom_histogram(binwidth = 25, fill = "#3b82f6", color = "white", alpha = 0.8) +
      geom_vline(xintercept = input$dist_budget, color = "#1d1d1d", linewidth = 1.2) +
      geom_vline(xintercept = med_price, color = "#b23a2f", linewidth = 1, linetype = "dashed") +
      scale_x_continuous(labels = dollar_format()) +
      theme_minimal()
  })
  
  output$dist_percentile <- renderText({
    data <- dist_data()
    req(nrow(data) > 0)
    paste0("Your budget is higher than about ", round(mean(data$price <= input$dist_budget, na.rm = TRUE) * 100), "% of listings.")
  })
  
  output$dist_summary <- renderText({
    data <- dist_data()
    req(nrow(data) > 0)
    paste0("Median price is $", round(median(data$price, na.rm = TRUE)), ".")
  })
  
  heatmap_data <- reactive({
    data <- df %>% filter(price > 0, price <= input$heat_price, minimum_nights <= input$heat_min_nights)
    if (input$heat_room != "All") data <- data %>% filter(room_type == input$heat_room)
    max_p <- ceiling(max(data$price, na.rm = TRUE) / 100) * 100
    data %>% mutate(price_bin = cut(price, breaks = seq(0, max_p, by = 100), include.lowest = TRUE)) %>%
      group_by(neighbourhood_group, price_bin) %>% summarise(avg_availability = mean(availability_365, na.rm = TRUE), .groups = "drop")
  })
  
  output$heatmap_plot <- renderPlotly({
    plot_ly(data = heatmap_data(), x = ~price_bin, y = ~neighbourhood_group, z = ~avg_availability, type = "heatmap", colors = colorRamp(c("#f7fbff", "#3b82f6", "#1e3a8a"))) %>%
      layout(xaxis = list(title = "Price Range"), yaxis = list(title = "Borough"))
  })
}

shinyApp(ui = ui, server = server)