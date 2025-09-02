########################################################
# Startup Funding Analysis & Dashboard (R Project)
# Author: Vasundhara Gour
# Libraries: tidyverse, lubridate, shiny, plotly, igraph, wordcloud
########################################################

# -------------------------------
# 1. Load Required Libraries
# -------------------------------
install.packages(c("tidyverse", "lubridate", "shiny", 
                   "plotly", "wordcloud", "igraph", "RColorBrewer"))
library(tidyverse)
library(lubridate)
library(shiny)
library(plotly)
library(wordcloud)
library(igraph)
library(RColorBrewer)
# Visualization
library(ggplot2)
install.packages("plotly")
library(plotly)       # for ggplotly()

# Wordcloud
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer) # for color palettes

# Network Graph
install.packages("igraph")
library(igraph)

# -------------------------------
# 2. Load & Clean Dataset
# -------------------------------
df <- read.csv("/Users/vasundharagoour/Downloads/startup_funding.csv",
               stringsAsFactors = FALSE)

# Rename columns for consistency
colnames(df) <- c("Sr_No", "Date", "Startup_Name", "Industry_Vertical", 
                  "SubVertical", "City", "Investors_Name", "Investment_Type", 
                  "Amount_USD", "Remarks")

# Convert date
df$Date <- dmy(df$Date)
df$Year <- year(df$Date)

# Clean funding amount
df$Amount_USD <- gsub(",", "", df$Amount_USD)
df$Amount_USD[df$Amount_USD %in% c("Undisclosed", "", "NA")] <- NA
df$Amount_USD <- as.numeric(df$Amount_USD)

# Handle missing values
df$Industry_Vertical[df$Industry_Vertical == "" | is.na(df$Industry_Vertical)] <- "Unknown"
df$City[df$City == "" | is.na(df$City)] <- "Unknown"

# Standardize text
df$Startup_Name <- str_to_title(trimws(df$Startup_Name))
df$City <- str_to_title(trimws(df$City))
df$Investment_Type <- str_to_title(trimws(df$Investment_Type))

# Remove invalid rows
df <- df %>% filter(!is.na(Date) & !is.na(Year))
df$Year <- as.numeric(df$Year)

summary(df)


# -------------------------------
# 3. Exploratory Data Analysis (EDA)
# -------------------------------

## (a) Funding trend by year
funding_trend <- df %>%
  group_by(Year) %>%
  summarise(Total_Funding = sum(Amount_USD, na.rm = TRUE), Deals = n())

ggplot(funding_trend, aes(x = Year, y = Total_Funding/1e9)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Total_Funding/1e9, 1)), vjust = -0.5) +
  labs(title = "Total Funding per Year", x = "Year", y = "Funding (Billion USD)") +
  theme_minimal()
ggsave(filename = "Documents/total_funding_per_year.png", plot = p, width = 8, height = 6, dpi = 300)

## (b) Top cities by funding
top_cities <- df %>%
  group_by(City) %>%
  summarise(Total_Funding = sum(Amount_USD, na.rm = TRUE)) %>%
  arrange(desc(Total_Funding)) %>%
  head(10)

ggplot(top_cities, aes(x = reorder(City, Total_Funding), y = Total_Funding/1e6)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 10 Cities by Funding", x = "City", y = "Funding (Million USD)") +
  theme_minimal()
ggsave(filename = "Documents/top_10_cities_funding.png", plot = p, width = 8, height = 6, dpi = 300)
## (c) Top industries by funding
top_industries <- df %>%
  group_by(Industry_Vertical) %>%
  summarise(Total_Funding = sum(Amount_USD, na.rm = TRUE)) %>%
  arrange(desc(Total_Funding)) %>%
  head(10)

ggplot(top_industries, aes(x = reorder(Industry_Vertical, Total_Funding), y = Total_Funding/1e6)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 10 Industries by Funding", x = "Industry", y = "Funding (Million USD)") +
  theme_minimal()
ggsave(filename = "Documents/top_10_industries_funding.png", plot = p, width = 8, height = 6, dpi = 300)
## (d) Investment type analysis
investment_types <- df %>%
  group_by(Investment_Type) %>%
  summarise(Total_Funding = sum(Amount_USD, na.rm = TRUE), Deals = n()) %>%
  arrange(desc(Total_Funding))

ggplot(investment_types, aes(x = reorder(Investment_Type, Total_Funding), y = Total_Funding/1e6)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Funding by Investment Type", x = "Investment Type", y = "Funding (Million USD)") +
  theme_minimal()
ggsave(filename = "Documents/funding_by_investment_type.png", plot = p, width = 8, height = 6, dpi = 300)
## (e) Top startups
top_startups <- df %>%
  group_by(Startup_Name) %>%
  summarise(Total_Funding = sum(Amount_USD, na.rm = TRUE)) %>%
  arrange(desc(Total_Funding)) %>%
  head(10)

ggplot(top_startups, aes(x = reorder(Startup_Name, Total_Funding), y = Total_Funding/1e6)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Top 10 Funded Startups", x = "Startup", y = "Funding (Million USD)") +
  theme_minimal()
ggsave(filename = "Documents/top_10_funded_startups.png", plot = p, width = 8, height = 6, dpi = 300)

# -------------------------------
# 4. Interactive Shiny Dashboard
# -------------------------------
ui <- fluidPage(
  titlePanel("Startup Funding Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Select City:", choices = unique(df$City), selected = "Bengaluru"),
      sliderInput("year", "Select Year Range:",
                  min = min(df$Year), max = max(df$Year),
                  value = c(min(df$Year), max(df$Year)), step = 1)
    ),
    
    mainPanel(
      plotOutput("fundingPlot"),
      plotOutput("industryPlot")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    df %>% filter(City == input$city, Year >= input$year[1], Year <= input$year[2])
  })
  
  output$fundingPlot <- renderPlot({
    funding_trend <- filtered_data() %>%
      group_by(Year) %>%
      summarise(Total_Funding = sum(Amount_USD, na.rm = TRUE))
    
    ggplot(funding_trend, aes(x = Year, y = Total_Funding/1e9)) +
      geom_col(fill = "steelblue") +
      labs(title = paste("Funding Trend in", input$city), y = "Funding (Billion USD)")
  })
  
  output$industryPlot <- renderPlot({
    top_industries <- filtered_data() %>%
      group_by(Industry_Vertical) %>%
      summarise(Total_Funding = sum(Amount_USD, na.rm = TRUE)) %>%
      arrange(desc(Total_Funding)) %>%
      head(10)
    
    ggplot(top_industries, aes(x = reorder(Industry_Vertical, Total_Funding), y = Total_Funding/1e6)) +
      geom_col(fill = "forestgreen") +
      coord_flip() +
      labs(title = "Top Industries", y = "Funding (Million USD)")
  })
}

shinyApp(ui = ui, server = server)


# -------------------------------
# 5. Extra Visualizations
# -------------------------------

## Interactive Plotly version of yearly funding
p <- ggplot(funding_trend, aes(x = Year, y = Total_Funding/1e9)) +
  geom_col(fill = "steelblue") +
  labs(title = "Total Funding per Year", y = "Funding (Billion USD)")
ggplotly(p)

## Wordcloud of investors
investors <- unlist(strsplit(df$Investors_Name, ","))
wordcloud(investors, max.words = 100, colors = brewer.pal(8, "Dark2"))

## Investor-Startup Network Graph
edges <- df %>% select(Startup_Name, Investors_Name) %>% na.omit()
g <- graph_from_data_frame(edges)
plot(g, vertex.size = 5, vertex.label = NA, edge.arrow.size = 0.3)

########################################################
# END OF PROJECT
########################################################
