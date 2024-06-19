required_packages <- c(
  "dplyr", "jsonlite", "leaflet", "shiny", "sf", "rnaturalearth", 
  "rnaturalearthdata", "ggplot2", "scales", "plotly", "readr", 
  "tidyr", "shinyjs", "shinyWidgets", "shinyBS", "shinythemes", "RColorBrewer"
)

# Function to install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if (length(new_packages)) {
    install.packages(new_packages)
  }
}
# Install any missing packages
install_if_missing(required_packages)
# Load all required packages
lapply(required_packages, library, character.only = TRUE)





library(dplyr)
library(jsonlite)
library(leaflet)
library(shiny)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(scales)
library(plotly)
library(readr)
library(tidyr)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(shinythemes)
library(RColorBrewer)


world_development_data <- read_csv("world_development_data_interpolated.csv")
life_expectancy <- read_csv("life expectancy.csv")
json_data <- fromJSON("country-codes-lat-long-alpha3.json")



safe_brewer_pal <- function(N, palette_name) {
  if (N < 2) {
    return(colorRampPalette(brewer.pal(3, palette_name))(N))
  } else {
    return(brewer.pal(N, palette_name))
  }
}



#wrangling


#correcting column names
clean_colnames <- function(data) {
  names(data) <- gsub(" ", "_", names(data))
  names(data) <- gsub("%", "pct", names(data))
  names(data) <- gsub("/", "_per_", names(data))
  names(data)
}
# Apply the cleaning function to both datasets
colnames(life_expectancy) <- clean_colnames(life_expectancy)
colnames(world_development_data) <- clean_colnames(world_development_data)
# Dropping extra columns
columns_to_drop <- c("IntermRegion", "SurfAreaSqKm", "FDINetBoP","PopDens",
                     "SubRegion" ,"GNI_per_CapAtlas", "GNIAtlas", "MobileSubs_per_100",
                     "InflConsPricpct")
# Drop the columns
world_development_data <- world_development_data[, !(names(world_development_data) %in% columns_to_drop)]
selected_columns <- c("Country_Name","Year","Region","Health_Expenditure_pct","Education_Expenditure_pct","Unemployment")
life_expectancy <- life_expectancy[ ,selected_columns]
# List of columns to apply median imputation
columns_median_impute <- c("Year","Country" , "Region", "PopGrowthpct","GDP","GDPGrowthpct","AdolFertRate","AgriValAddpctGDP","DomCreditpctGDP",
                           "ExportspctGDP" ,"FertRate" ,"GrossCapFormpctGDP","ImportspctGDP","LifeExpBirth","MerchTradepctGDP" ,"MilExppctGDP" ,
                           "MortRateU5"  ,"NetMigr" ,"PopTotal","RevenueExGrantspctGDP" ,"SchEnrollPrimpct","TaxRevenuepctGDP","UrbanPopGrowthpct","IndValAddpctGDP")
# Applying median imputation to each column
for (col in columns_median_impute) {
  if (col %in% names(world_development_data)) {
    median_value <- median(world_development_data[[col]], na.rm = TRUE)
    world_development_data[[col]][is.na(world_development_data[[col]])] <- median_value
  }}
life_expectancy$Health_Expenditure_pct[is.na(life_expectancy$Health_Expenditure_pct)] <- mean(life_expectancy$Health_Expenditure_pct, na.rm = TRUE)
life_expectancy$Education_Expenditure_pct[is.na(life_expectancy$Education_Expenditure_pct)] <- mean(life_expectancy$Education_Expenditure_pct, na.rm = TRUE)
life_expectancy$Unemployment[is.na(life_expectancy$Unemployment)] <- mean(life_expectancy$Unemployment, na.rm = TRUE)

# Function to compute outliers using IQR
compute_outliers <- function(data) {
  outliers <- list()
  numeric_columns <- sapply(data, is.numeric)
  for (column in names(data)[numeric_columns]) {
    Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outlier_count <- sum(data[[column]] < lower_bound | data[[column]] > upper_bound, na.rm = TRUE)
    outliers[[column]] <- outlier_count
  }
  return(outliers)
}
# Compute outliers for both datasets
world_development_outliers <- compute_outliers(world_development_data)
life_expectancy_outliers <- compute_outliers(life_expectancy)

#fixing outliers
# Function to cap and floor outliers for all numeric columns
cap_floor_all_columns <- function(data) {
  numeric_columns <- sapply(data, is.numeric)  # Identify numeric columns
  for (column in names(data)[numeric_columns]) {
    Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Apply capping and flooring
    data[[column]] <- ifelse(data[[column]] < lower_bound, lower_bound,
                             ifelse(data[[column]] > upper_bound, upper_bound, data[[column]]))
  }
  return(data)
}
# Apply to the entire World Development Data
world_development_data <- cap_floor_all_columns(world_development_data)
# Apply to the entire Life Expectancy Data
life_expectancy <- cap_floor_all_columns(life_expectancy)


num_duplicates_world_dev = sum(duplicated(world_development_data))
# Count duplicate rows in the Life Expectancy Data
num_duplicates_life_exp = sum(duplicated(life_expectancy))
world_development_data$MigrImpact <- (world_development_data$NetMigr / world_development_data$PopTotal) * 100


#world_development_data -first file

#life expectancy
country_mapping <- list(
  "Egypt, Arab Rep." = "Egypt",
  "Viet Nam" = "Vietnam",
  "Cabo Verde" = "Cape Verde",
  "Venezuela, RB" = "Venezuela",
  "Slovak Republic" = "Slovakia",
  "Korea, Rep." = "South Korea",
  "Yemen, Rep." = "Yemen",
  "Macao SAR, China" = "Macau",
  "Turkiye" = "Turkey",
  "Gambia, The" = "Gambia",
  "Congo, Rep." = "Republic of Congo",
  "Congo, Dem. Rep." = "Democratic Republic of the Congo",
  "Russian Federation" = "Russia",
  "Iran, Islamic Rep." = "Iran",
  "Lao PDR" = "Laos",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Czechia" = "Czech Republic",
  "Syrian Arab Republic" = "Syria",
  "Brunei Darussalam" = "Brunei"
  # Add more mappings as necessary
)

# Filter both datasets for the years 2001 to 2019
world_development_datanew <- filter(world_development_data, Year >= 2001 & Year <= 2019)
life_expectancy <- filter(life_expectancy, Year >= 2001 & Year <= 2019)
# Apply the mapping to both datasets
world_development_datanew$Country <- recode(world_development_datanew$Country, !!!country_mapping)
life_expectancy$Country_Name <- recode(life_expectancy$Country_Name, !!!country_mapping)

# Ensure columns are consistent for merging
world_development_datanew <- rename(world_development_datanew, Country_Name = Country)
world_development_datanew <- rename(world_development_datanew, Region_World_Dev = Region)

# Perform the merge
world_development_life_expectancy<- merge(world_development_datanew, life_expectancy, by = c("Country_Name", "Year"), all = TRUE)
world_development_life_expectancy <- na.omit(world_development_life_expectancy)



#second file-world_development_life_expectancy

# Convert JSON data to a dataframe
json_df <- data.frame(
  country = json_data$ref_country_codes$country,
  longitude = json_data$ref_country_codes$longitude,
  latitude = json_data$ref_country_codes$latitude
)

# Create a mapping dataframe to correct non-matching country names
mapping <- data.frame(
  json_name = c("Bolivia, Plurinational State of", "Cape Verde", "Czech Republic", "Egypt",
                "Iran, Islamic Republic of", "Macao", "Macedonia, the former Yugoslav Republic of",
                "Micronesia, Federated States of", "Slovakia", "Venezuela, Bolivarian Republic of",
                "Yemen", "Congo, the Democratic Republic of the", "Côte d'Ivoire", "Korea, Republic of",
                "Lao People's Democratic Republic", "Moldova, Republic of", "Tanzania, United Republic of",
                "Viet Nam", "Syrian Arab Republic", "Hong Kong", "Libyan Arab Jamahiriya",
                "Swaziland", "United States Minor Outlying Islands", "Western Sahara", 
                "Palestinian Territory, Occupied", "Russian Federation",
                "Korea, Democratic People's Republic of", "Northern Mariana Islands",
                "Netherlands Antilles"),
  csv_name = c("Bolivia", "Cabo Verde", "Czechia", "Egypt, Arab Rep.", "Iran, Islamic Rep.",
               "Macao SAR, China", "North Macedonia", "Micronesia, Fed. Sts.", "Slovak Republic",
               "Venezuela, RB", "Yemen, Rep.", "Congo, Dem. Rep.", "Cote d'Ivoire", "Korea, Rep.",
               "Lao PDR", "Moldova", "Tanzania", "Vietnam", "Syria", "Hong Kong SAR, China", 
               "Libya", "Eswatini", "United States", "West Bank and Gaza", "Russia", 
               "North Korea", "Syria", "Northern Mariana Islands", "Curacao")
)

# Update JSON dataframe using the mapping
json_df <- json_df %>%
  left_join(mapping, by = c("country" = "json_name")) %>%
  mutate(country = ifelse(is.na(csv_name), country, csv_name)) %>%
  select(-csv_name)

# Ensure json_df has unique countries
json_df <- json_df %>%
  distinct(country, .keep_all = TRUE)

# Merge the updated JSON dataframe with the CSV dataframe
merged_data <- left_join(world_development_data, json_df, by = c("Country" = "country"))

# Proceed with additional operations if debugging is successful
countries <- ne_countries(scale = "medium", returnclass = "sf")
merged_data <- left_join(merged_data, countries %>% select(name, geometry), by = c("Country" = "name"))
merged_data <- st_as_sf(merged_data, sf_column_name = "geometry")
merged_data <- merged_data %>% filter(!is.na(geometry) & !st_is_empty(geometry))
merged_data$Year <- as.numeric(as.character(merged_data$Year))
#third file-merged_data json+csv








#aggregation and calculations
region_focus <- list(
  Asia = list(lat = 34, lng = 100, zoom = 3),
  Europe = list(lat = 50, lng = 10, zoom = 3),
  Africa = list(lat = 0, lng = 20, zoom = 3),
  Americas = list(lat = 10, lng = -75, zoom = 3),
  Oceania = list(lat = 10, lng = -75, zoom = 0)
)

# using  world_development_life_expectancy
population_data <- world_development_life_expectancy
population_data <- population_data %>%
  arrange(Year, Region_World_Dev) %>%
  group_by(Region_World_Dev) %>%
  mutate(PrevYearPop = lag(PopTotal)) %>%
  ungroup()

# Calculating correlation coefficients for each region
correlation_data <- population_data %>%
  group_by(Region_World_Dev) %>%
  summarize(Correlation = cor(NetMigr, PopTotal, use = "complete.obs", method = "pearson"),
            .groups = 'drop')


# Calculate average net migration and population total by region and year
agg_data <- population_data %>%
  group_by(Region_World_Dev, Year) %>%
  summarize(Avg_NetMigr = mean(NetMigr, na.rm = TRUE),
            Avg_PopTotal = mean(PopTotal, na.rm = TRUE),
            .groups = 'drop') %>%
  left_join(correlation_data, by = "Region_World_Dev")








#ui  Apply the Flatly theme 

ui <- fluidPage(
  theme = shinytheme("flatly"),  
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .full-page {
        height: 100vh;
        overflow-y: auto;
        padding: 40px;
      }
      .nav-buttons {
        position: fixed;
        bottom: 10px;
        right: 10px;
        z-index: 1000;
      }
      .btn-primary {
        background-color: #4CAF50; /* Green */
        border: none;
        color: white;
        padding: 15px 32px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 16px;
        margin: 4px 2px;
        cursor: pointer;
      }
      h1, h2, h3 {
        color: #333; /* Dark grey */
      }
    "))
  ),
  
  
  
  # Page 1: Introduction and Key Questions
  div(id = "page1", class = "full-page",
      titlePanel(h1("Global Shifts: Analyzing GDP Dynamics, Population Growth, and Migration Influences Over Time", style = "text-align: center;")),
      h2("Aim and Target Audience", id = "title_section"),
      p(class = "description-box",
        "This research aims to dissect the intricate relationships between economic growth, 
        population dynamics, and migration patterns in different regions(Asia,Europe,Africa,Americas,Oceania), 
        offering valuable insights  into the drivers of economic development and how it changed over time
        , the impact of migration on population trends, 
        and the role of socio-economic factors in shaping migration decisions. By focusing on these aspects, 
        the study seeks to illuminate the complex interplay between economic opportunities and demographic 
        changes across different global regions, providing a comprehensive overview that policymakers, 
        academic researchers, and development agencies can utilize to foster sustainable development 
        and address regional disparities. Additionally, the findings from this study serve as a crucial
        resource for students in related fields, enhancing their understanding and providing a solid basis
        for academic projects and research endeavors. 
        
        .Through this research, we aim to equip our audience with the
        knowledge needed to implement informed policies that effectively tackle the pressing issues of migration, 
        population management, and economic inequality, thereby contributing to a more equitable global landscape."),
      hr(),
      h3("Core Questions Explored ", id = "questions_section"),
      wellPanel(
        tags$ul(
          tags$li("We aim to answer these questions from this project."),
          tags$li("What are the most significant factors influencing economic growth and development in different regions, and how do these factors vary over time?"),
          tags$li("How does the net migration rate (NetMigr) impact the total population (PopTotal) growth in different regions over time.?"),
          tags$li("How do investments in healthcare and education, as well as fluctuations in unemployment rates, influence migration patterns (2001-2019)?")
        )
      ),
      div(class = "nav-buttons", actionButton("toNext1", "Next Section", class = "btn btn-primary"))
  ),
  
  # Page 2: Map and GDP Chart with Controls
  hidden(
    div(id = "page2", class = "full-page",
        fluidRow(
          column(12, 
                 div(style = "text-align: right; margin-bottom: 20px;", 
                     actionButton("showSummary", "Show Summary", icon = icon("list-alt", "fa-2x"), class = "btn btn-primary btn-lg")
                 )
          )
        ),
        fluidRow(
          column(6, h2("Economic Growth Map", style = "text-align: center;"), leafletOutput("map", height = "550px")),
          column(6, h2("GDP Trend Line Chart", style = "text-align: center;"), plotlyOutput("gdpPlot", height = "550px"))
        ),
        
        fluidRow(
          column(6, selectInput("region", "Select Region:", choices = unique(merged_data$Region))),
          column(6, sliderInput("year", "Select Year:", min = min(merged_data$Year, na.rm = TRUE), max = max(merged_data$Year, na.rm = TRUE), value = min(merged_data$Year), step = 1, animate = animationOptions(interval = 1000, loop = FALSE, playButton = icon("play"), pauseButton = icon("pause")))),
          column(width = 10, verbatimTextOutput("Map"))
          
        ),
        
        div(class = "nav-buttons", actionButton("toPrevious", "Main Page", class = "btn btn-primary"),actionButton("toNext2", "Next Section", class = "btn btn-primary")
        )
    )
  ),
  
  # Page 3: Correlation with GDP Growth
  hidden(
    div(id = "page3", class = "full-page",
        fluidRow(
          column(12, 
                 div(style = "text-align: right; margin-bottom: 20px;", 
                     actionButton("summary_button1", "Show Summary", icon = icon("list-alt", "fa-2x"), class = "btn btn-primary btn-lg")
                 )
          )
        ),
        h1("Significant Factors Influencing Economic Growth", id = "correlation_section"),
        plotlyOutput("correlation_plot"),
        fluidRow(
          column(8, 
                 div(style = "text-align: right; margin-bottom: 20px;", 
                     actionButton("show_time_series", "Show Detailed Time Series Analysis", icon = icon("list-alt", "fa-2x"), class = "btn btn-primary btn-lg")
                 )
          )
        ),
        
        column(width = 12, verbatimTextOutput("co_relation")),
        div(id = "dynamic_time_series", hidden = TRUE,
            h2("Dynamic Economic Factors Analysis Over Time"),
            fluidRow(
              column(4, checkboxGroupInput("regionInput", "Choose Region:", choices = unique(world_development_data$Region), selected = "Asia")),
              column(4, selectInput("factorInput", "Select Economic Factor:", choices = c('GrossCapFormpctGDP', 'UrbanPopGrowthpct', 'PopGrowthpct', 'MerchTradepctGDP', 'AgriValAddpctGDP', 'ImportspctGDP', 'ExportspctGDP', 'MerchTradepctGDP'))),
              column(4, sliderInput("yearRange", "Select Year Range:", min = min(world_development_data$Year, na.rm = TRUE), max = max(world_development_data$Year, na.rm = TRUE), value = c(1973, 1978), step = 1, animate = animationOptions(interval = 1000, loop = TRUE, playButton = "Play", pauseButton = "Pause")))
            ),
            plotlyOutput("timeSeriesPlot"),
        ),
        div(class = "nav-buttons", actionButton("toPrevious2", "Back to Map", class = "btn btn-primary"), actionButton("toNext3", "Next Section", class = "btn btn-primary"))
    )
  ),
  
  
  
  # Page 4: Heatmap Analysis
  hidden(
    div(id = "page4", class = "full-page",
        h1("Net Migration Rate Impact on Population Growth by Region"),
        fluidRow(
          column(12, 
                 div(style = "text-align: right; margin-bottom: 20px;", 
                     actionButton("heatmapSummary", "Show Heatmap Summary", icon = icon("list-alt", "fa-2x"), class = "btn btn-primary btn-lg")
                 )
          )
        ),
        
        fluidRow(
          column(10, plotlyOutput("heatmap", height = "600px")),
        ),
        column(10, verbatimTextOutput("heatmapDesc")),
        div(class = "nav-buttons", actionButton("toPrevious4", "Back to Time Series Analysis", class = "btn btn-primary"), actionButton("toNext4", "Next Section", class = "btn btn-primary"))
    )
  ),
  
  
  
  # Page 5: Population Analysis and Socio-Economic Indicators
  hidden(
    div(id = "page5", class = "full-page",
        fluidRow(
          column(12, 
                 div(style = "text-align: right; margin-bottom: 20px;", 
                     actionButton("summaryButton", "Show Summary", icon = icon("list-alt", "fa-2x"), class = "btn btn-primary btn-lg")
                 )
          )
        ),
        fluidRow(
          column(6, h1("Population Analysis by Region"), plotlyOutput("bubbleChart", width = "100%", height = "550px")),
          column(6, h1("Socio-Economic Indicators by Region"), plotOutput("indicatorPlot", width = "100%", height = "550px"))
        ),
        fluidRow(
          column(4, selectInput("region2", "Select Region:", choices = c("All" = "All", "Africa" = "Africa", "Americas" = "Americas", "Asia" = "Asia", "Europe" = "Europe", "Oceania" = "Oceania"))),
          column(3, sliderInput("yearSlider", "Select Year:", min = min(population_data$Year), max = max(population_data$Year), value = min(population_data$Year), step = 1, width = '100%')),
          column(1, actionButton("playButton", "Play/Pause", icon = icon("play-circle"))),
          column(1, actionButton("stopButton", "Stop", icon = icon("stop-circle"))),
        ),
        fluidRow(
          column(10, htmlOutput("linkdesc")),
          div(class = "nav-buttons", actionButton("toPrevious5", "Back to Heatmap", class = "btn btn-primary"), actionButton("toMainPage", "Main Page", class = "btn btn-primary"))
        ),
    )
  )
)






# Define Server
server <- function(input, output, session) {
  
  # Forward navigation events
  observeEvent(input$toNext1, {
    shinyjs::hide("page1")
    shinyjs::show("page2")
  })
  observeEvent(input$toNext2, {
    shinyjs::hide("page2")
    shinyjs::show("page3")
  })
  observeEvent(input$show_time_series, {
    shinyjs::toggle("dynamic_time_series")
  })
  observeEvent(input$toNext3, {
    shinyjs::hide("page3")
    shinyjs::show("page4")
  })
  observeEvent(input$toNext4, {
    shinyjs::hide("page4")
    shinyjs::show("page5")
  })
  observeEvent(input$toNext5, {
    shinyjs::hide("page5")
    shinyjs::show("page1")
  })
  
  
  # Back navigation events
  observeEvent(input$toPrevious2, {
    shinyjs::hide("page3")
    shinyjs::show("page2")
  })
  observeEvent(input$toPrevious3, {
    if (shinyjs::isHidden("dynamic_time_series")) {
      shinyjs::hide("page3")
      shinyjs::show("page2")
    } else {
      shinyjs::hide("dynamic_time_series")
    }
  })
  observeEvent(input$toPrevious4, {
    shinyjs::hide("page4")
    shinyjs::show("page3")
  })
  observeEvent(input$toPrevious, {
    shinyjs::hide("page2")
    shinyjs::show("page1")
  })
  observeEvent(input$toPrevious5, {
    shinyjs::hide("page5")
    shinyjs::show("page4")
  })
  
  # Navigation to the main page
  observeEvent(input$toMainPage, {
    shinyjs::hide(c("page5"))
    shinyjs::show("page1")
  })
  
  
  
  
  #map and line chart
output$map <- renderLeaflet({
    data <- merged_data %>%
      filter(Region == input$region, Year == input$year) %>%
      filter(!is.na(longitude) & !is.na(latitude) & !is.na(GDPGrowthpct))
    
    pal <- colorNumeric(palette = safe_brewer_pal(3, "RdYlBu"), domain = na.omit(data$GDPGrowthpct), na.color = "transparent")
    
    leaflet(data) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = region_focus[[input$region]]$lng, lat = region_focus[[input$region]]$lat, zoom = region_focus[[input$region]]$zoom) %>%
      addPolygons(
        fillColor = ~pal(GDPGrowthpct),
        fillOpacity = 0.7,
        color = "#444444",
        weight = 2,
        opacity = 1,
        popup = ~paste("<strong>Country:</strong>", Country,
                       "<br><strong>Year:</strong>", Year,
                       "<br><strong>GDP Growth:</strong>", sprintf("%.2f%%", GDPGrowthpct)),
        label = ~paste("Country:", Country, "GDP Growth:", sprintf("%.2f%%", GDPGrowthpct)),
        labelOptions = labelOptions(
          direction = 'auto',
          noHide = FALSE,
          textOnly = TRUE,
          style = list("color" = "black", "font-weight" = "bold", "font-size" = "12px")
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~GDPGrowthpct,
                title = "GDP Growth %", labFormat = labelFormat(suffix = "%"), opacity = 0.7)
  })
  
  
  output$gdpPlot <- renderPlotly({
    data_summary <- reactive({
      df <- merged_data %>%
        filter(Region == input$region) %>%
        group_by(Year, Region) %>%
        summarize(GDP = sum(GDP, na.rm = TRUE), .groups = 'drop') %>%
        arrange(Year) %>%
        mutate(
          PreviousGDP = lag(GDP),
          Change = GDP - PreviousGDP,
          PercentChange = ((GDP / PreviousGDP - 1) * 100),
          Direction = if_else(Change > 0, "increased", if_else(Change < 0, "decreased", "stable"))  # Ensure handling of zero change
        )
      
      # Handle the case where the first year has NA values for changes
      df$Change[is.na(df$Change)] <- 0
      df$PercentChange[is.na(df$PercentChange)] <- 0
      
      df
    })
    
    if (nrow(data_summary()) == 0) return(NULL)
    
    p <- ggplot(data_summary(), aes(x = Year, y = GDP, text = paste("Year:", Year, "<br>GDP:", scales::dollar(GDP), "<br>Change:", scales::dollar(Change), Direction, sprintf("%.2f%%", PercentChange)))) +
      geom_line(size = 1.5, aes(group = Region, color = Region)) +
      scale_x_continuous(
        breaks = seq(from = 1973, to = 2021, by = 4),  # Show a year mark every 5 years
        labels = seq(from = 1973, to = 2021, by = 4)  # Adjust labels to match the breaks
      )+
      labs(title = "Global GDP Trends by Region", x = "Year", y = "Global GDP") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = "text")
  })
  output$Map <- renderText({
    " Detailed Summary
-Asia
2018-2020 Analysis: Despite initial setbacks due to the pandemic, Asia's economies, particularly East Asia, were quick to initiate recovery processes by late 2020.
2021 Recovery: Led by China's early containment of the virus and subsequent economic reopening, the region saw a quick rebound.
Impact Reason: Manufacturing and export disruptions initially slowed growth, but effective pandemic management helped in recovery. 
-Europe
2018-2020 Analysis: The region was hit hard by COVID-19, especially countries in the Eurozone, leading to recessions in several economies.
2021 Recovery: The recovery was facilitated by strong support measures from the European Union and aggressive vaccination drives.
Impact Reason: Widespread lockdowns and the disruption of trade within the EU and with global partners were primary downturn causes.
-Africa
2018-2020 Analysis: Economic growth stalled significantly in 2020 due to the pandemic's impact on global commodity prices and internal strife.
2021 Recovery: The recovery has been slow due to challenges in vaccine distribution and continued geopolitical tensions.
Impact Reason: Heavy reliance on commodity exports and insufficient healthcare infrastructure hindered rapid response to economic shocks.
-Americas
2018-2020 Analysis: The region faced a severe economic contraction in 2020 due to the pandemic, with widespread lockdowns and a collapse in consumer demand.
2021 Recovery: Recovery in 2021 was uneven, with the U.S. showing strong rebound effects thanks to substantial fiscal stimuli.
Impact Reason: The pandemic's impact was exacerbated by political instability and pre-existing economic vulnerabilities.
-Oceania
2018-2020 Analysis: The region saw disruptions in 2020 due to COVID-19, impacting key sectors like tourism and exports, primarily driven by Australia and New Zealand.
2021 Recovery: In 2021, Oceania began to show signs of recovery, aided by effective pandemic management and stimulus measures.
Impact Reason: The tourism and export sectors were heavily affected by global lockdowns and travel bans.
"
  })
  
  
  observeEvent(input$showSummary, {
    showModal(modalDialog(
      title = "Visualizations Summary",
      HTML("
      <h3>General Trend:</h3>
      <ul>
        <li><strong>Asia:</strong> Asia's chart is characterized by the most dynamic and rapid growth, reflecting its emerging market vigor and economic diversification. China and India significantly influence Asia's GDP due to their large economies, population size, and manufacturing capabilities.</li>
        <li><strong>Europe:</strong> Europe shows a steady growth pattern with resilience in facing economic downturns. Germany and France, as leading economies, play critical roles in shaping the economic landscape of Europe.</li>
        <li><strong>Africa:</strong> Africa's GDP growth has been on an upward trend, with significant acceleration in the past two decades. Nigeria and South Africa, as major economies, have had substantial impacts on the region's economic performance.</li>
        <li><strong>Americas:</strong> The Americas have experienced robust growth, with a strong upward trajectory over the decades. The United States and Brazil have been pivotal, with the U.S. economy having a major influence on the regional and global economic climates.</li>
        <li><strong>Oceania:</strong> Oceania's economic graph shows steady growth with occasional fluctuations, marked by sharp recoveries post-downturns. Australia and New Zealand are crucial drivers of Oceania's GDP, significantly influencing regional economic trends.</li>
      </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  
  
# Correlation plot rendering
  factors <- c('GrossCapFormpctGDP', 'UrbanPopGrowthpct', 'PopGrowthpct', 'MerchTradepctGDP', 'AgriValAddpctGDP',
               'ImportspctGDP', 'ExportspctGDP', 'MerchTradepctGDP')
  correlations <- c(0.182808, 0.160560, 0.136787, 0.102857, 0.089188, 
                    -0.45213450, -0.31354480, -0.33643022)
  descriptions <- c("GrossCapFormpctGDP:Represents total investment in physical assets like machinery, buildings, and infrastructure within a country.",
                    "UrbanPopGrowthpct:Indicates the rate at which the urban population is increasing",
                    "PopGrowthpct:Reflects the annual increase in population",
                    "MerchTradepctGDP:Measures the total value of a country's import and export activities",
                    "AgriValAddpctGDP:Represents agriculture's contribution to the national economic output",
                    "ImportspctGDP:Quantifies the imports' value relative to the total GDP",
                    "ExportspctGDP:Shows the proportion of a country’s total production that is sold abroad",
                    "Evaluates the size of both imports and exports in relation to GDP")
  data_corr <- data.frame(Factors = factors, Correlations = correlations, Descriptions = descriptions)
  
  p <- ggplot(data_corr, aes(x = Factors, y = Correlations, fill = Correlations > 0, text = paste("Factor: ", Descriptions, "<br>Correlation: ", Correlations))) +
    geom_bar(stat = 'identity') +
    labs(title = "Correlation of Various Factors with GDP Growth",
         x = "Factors", y = "Correlation with GDP Growth") +
    coord_flip() +
    scale_fill_manual(values = c("red", "green"))
  
  output$correlation_plot <- renderPlotly({
    ggplotly(p, tooltip = "text")
  })
  output$co_relation <- renderText({ "To see how various economic factors correlate with GDP growth over different years click the above -Show detailed timeseries Analysis button a new vis will appear or click on summary to see more information."})
  
  output$timeSeriesPlot <- renderPlotly({
    filtered_data <- world_development_data %>%
      filter(Region %in% input$regionInput, Year >= input$yearRange[1], Year <= input$yearRange[2]) %>%
      select(Year, Region, !!sym(input$factorInput))
    
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Return NULL to prevent Plotly from trying to plot empty data
    }
    
    long_data <- pivot_longer(filtered_data, cols = !!sym(input$factorInput), names_to = "Factor", values_to = "Value") %>%
      group_by(Region, Factor) %>%
      arrange(Region, Year) %>%
      mutate(PrevYearValue = lag(Value),
             Change = Value - PrevYearValue,
             IncreaseDecrease = case_when(
               Change > 0 ~ "increased",
               Change < 0 ~ "decreased",
               TRUE ~ "stayed the same"
             ))
    
    if (nrow(long_data) == 0) {
      return(NULL)  # Return NULL if no data after processing
    }
    
    plot_ly(data = long_data, x = ~Year, y = ~Value, color = ~Region, type = 'scatter', mode = 'lines+markers', fill = 'tozeroy',
            text = ~paste("Year: ", Year,
                          "<br>Region: ", Region,
                          "<br>Value: ", Value,
                          "<br>Previous Year Value: ", PrevYearValue,
                          "<br>Change: ", IncreaseDecrease),
            hoverinfo = "text") %>%
      layout(title = paste("Yearly Trends for", input$factorInput, "by Region"),
             xaxis = list(title = "Year"),
             yaxis = list(title = paste(input$factorInput, "Index")))
  })
  
  
  observeEvent(input$summary_button1, {
    showModal(modalDialog(
      title = "Summary",
      HTML("
      <p>Each bar represents an economic indicator with its correlation to GDP growth depicted by its length and color—green for positive and red for negative correlations. Hover over each bar to get detailed descriptions of each factor and understand their potential impact on economic growth. The detailed analysis shows how factors change over time.</p>
      <p>By choosing a region, factor, and time, you can see how it increased or decreased in that region.</p>
      <p>This visualization below shows how much the factor increased or decreased from last year.</p>
    "),
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  
  

#heatmap
  output$heatmap <- renderPlotly({
    p <- plot_ly(agg_data, x = ~Year, y = ~Region_World_Dev, type = "heatmap",
                 z = ~Avg_NetMigr, colors = viridis::viridis(100),
                 text = ~paste("Year:", Year,
                               "<br>Region:", Region_World_Dev,
                               "<br>Average Migration:", format(Avg_NetMigr, big.mark = ","),
                               "<br>Average Population:", format(Avg_PopTotal, big.mark = ","),
                               "<br>Correlation (NetMig & Pop):", sprintf("%.2f", Correlation),
                               "<br>Impact:", ifelse(Avg_NetMigr >= 0, "Positive", "Negative")),
                 hoverinfo = "text",
                 colorbar = list(title = "Avg Net Migration")) %>%
      layout(title = "Net Migration Rate Impact on Population Growth by Region",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Region"))
    
    return(p)
  })
  
output$heatmapDesc <- renderText({
    "
How it can help-
first- put cursor on heatmap to see positive and negative effects of 
This heatmap helps identify regions needing intervention based on migration trends and their impact on population growth. 
It allows users to explore correlations and trends, generating hypotheses about the drivers of migration and their effects. 
The visualization provides insights into the evolution of migration patterns over time and their implications for population 
dynamics.
Intensity of different colors-
Color Intensity:The heatmap uses a gradient color scale to indicate the magnitude of net migration.
Positive Migration (Green to Yellow): Indicates regions and years where net migration was positive, suggesting an influx of people.
Negative Migration (Blue to Purple): Indicates regions and years where net migration was negative, suggesting an outflow of people
"
  })
  
  observeEvent(input$heatmapSummary, {
    showModal(modalDialog(
      title = "Heatmap Summary",
      HTML("
      <p>Net migration, reflecting the balance of people entering and leaving a country, notably shows distinct patterns across different regions:</p>
      <ul>
        <li><strong>Asia:</strong> Despite high net migration rates, population growth persists due to large populations and high fertility rates in countries like India and China. This highlights a complex relationship where high net migration does not significantly impact overall population growth.</li>
        <li><strong>Europe:</strong> Positive net migration correlates with steady population growth, indicating that as more people migrate into the region, the total population tends to increase.</li>
        <li><strong>Africa:</strong> Exhibits mixed patterns where both positive and negative net migration rates are seen. The impact on population growth varies, reflecting diverse migration dynamics across the continent.</li>
        <li><strong>Americas:</strong> Generally, higher net migration is associated with increased population growth, particularly in North America, driven by substantial immigration rates.</li>
        <li><strong>Oceania:</strong> The region shows positive correlations, indicating that higher net migration rates contribute to population growth, particularly in Australia and New Zealand.</li>
      </ul>
      <p>The Pearson's correlation coefficient test for different regions reveals varying relationships between net migration and population growth:</p>
      <ul>
        <li>Positive correlations in regions like Europe and Oceania indicate that as net migration rates increase, total population growth tends to rise.</li>
        <li>Negative correlations in regions like Africa and the Americas suggest that higher net migration rates are associated with decreased population growth.</li>
      </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  
  
  
  
#BUBBLE AND MULTIPANEL CHARTS
  
  output$bubbleChart <- renderPlotly({
    plot_data <- reactive_data()
    p <- ggplot(plot_data, aes(x = Region_World_Dev, y = PopTotal, size = PopTotal, color = Region_World_Dev,
                               text = paste("Region:", Region_World_Dev, 
                                            "<br>Population:", PopTotal, 
                                            "<br>Change from last year:", Change, 
                                            ifelse(is.na(Change), "", ifelse(Change >= 0, "(Increased)", "(Decreased)")),
                                            "<br>Year:", input$yearSlider))) +
      geom_point(alpha = 0.5) +
      scale_size(range = c(5, 50), name = "Population Size") +
      labs(title = paste("Population by Region in Year", input$yearSlider),
           x = "Region",
           y = "Total Population") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
#multipanel
  output$indicatorPlot <- renderPlot({
    filtered_data <- population_data %>%
      filter(Year == as.numeric(input$yearSlider))
    
    if (nrow(filtered_data) == 0) {
      print("No data available for the selected year.")
      return()
    }
    
    grouped_data <- filtered_data %>%
      group_by(Region_World_Dev) %>%
      summarise(
        Health_Expenditure = mean(Health_Expenditure_pct, na.rm = TRUE),
        Education_Expenditure = mean(Education_Expenditure_pct, na.rm = TRUE),
        Unemployment = mean(Unemployment, na.rm = TRUE),
        Net_Migration = mean(MigrImpact, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = c(Health_Expenditure, Education_Expenditure, Unemployment, Net_Migration), names_to = "Variable", values_to = "Value") %>%
      mutate(Alpha = if_else(Region_World_Dev == input$region2 | input$region2 == "All", 1, 0.2))
    
    if (nrow(grouped_data) == 0) {
      print("No data to display after processing.")
      return()
    }
    
    p <- ggplot(grouped_data, aes(x = Region_World_Dev, y = Value, fill = Region_World_Dev, alpha = Alpha)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~ Variable, scales = "free_y") +
      theme_minimal() +
      labs(title = paste("Socio-Economic Indicators by Region in", input$yearSlider), x = "Region", y = "Value") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_alpha_identity()
    
    print(p)  # Debugging: output the plot object to the console
  })
  
  observeEvent(input$summaryButton, {
    showModal(modalDialog(
      title = "Summary",
      HTML("
      <h3>Bubble Chart: Population by Region</h3>
      <p><strong>Purpose:</strong> Display total population by region for a selected year.</p>
      <p><strong>Aggregation:</strong> Data aggregated by region and year, showing total population and changes from the previous year.</p>
      <p><strong>Insights:</strong></p>
      <ul>
        <li>Compare population sizes across regions.</li>
        <li>Observe population changes from the previous year.</li>
        <li>Bubble sizes correspond to total population, making it clear which regions have larger or smaller populations.</li>
      </ul>
      <h3>Multi-Panel Chart: Socio-Economic Indicators by Region</h3>
      <p><strong>Purpose:</strong> Present various socio-economic indicators by region for a selected year.</p>
      <p><strong>Aggregation:</strong> Data aggregated by region and year, showing health expenditure, education expenditure, unemployment, and net migration impact.</p>
      <p><strong>Insights:</strong></p>
      <ul>
        <li>Highlight differences in health and education expenditures across regions.</li>
        <li>Identify regions with high or low unemployment rates.</li>
        <li>Understand the impact of net migration on each region.</li>
        <li>Compare socio-economic indicators across regions to understand disparities.</li>
      </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  
  
  
  
  
  output$linkdesc <- renderUI({
    HTML(
      "Source1: <a href='https://www.kaggle.com/datasets/samybaladram/databank-world-development-indicators?select=world_development_data_interpolated.csv' target='_blank'>https://www.kaggle.com/datasets/samybaladram/databank-world-development-indicators?select=world_development_data_interpolated.csv</a><br>
    Source2: <a href='https://github.com/eesur/country-codes-lat-long/blob/master/country-codes-lat-long-alpha3.json' target='_blank'>https://github.com/eesur/country-codes-lat-long/blob/master/country-codes-lat-long-alpha3.json</a><br>
    Source3: <a href='https://www.kaggle.com/datasets/mjshri23/life-expectancy-and-socio-economic-world-bank?rvi=1' target='_blank'>https://www.kaggle.com/datasets/mjshri23/life-expectancy-and-socio-economic-world-bank?rvi=1</a>"
    )
  })
  
  
  
  
  
  
  # Reactive expression to filter data based on the selected year and calculate changes
  reactive_data <- reactive({
    year_data <- population_data %>%
      filter(Year == input$yearSlider)
    
    if (input$yearSlider > min(population_data$Year)) {
      year_data <- year_data %>%
        mutate(Change = PopTotal - PrevYearPop)
    } else {
      year_data <- year_data %>%
        mutate(Change = NA)
    }
    
    year_data %>%
      group_by(Region_World_Dev) %>%
      summarize(PopTotal = sum(PopTotal, na.rm = TRUE),
                Change = sum(Change, na.rm = TRUE),
                .groups = 'drop')
  })
  
  
  # Animation control
  animate <- reactiveVal(FALSE)
  
  observeEvent(input$playButton, {
    if (animate()) {
      animate(FALSE)
      updateActionButton(session, "playButton", label = "Play", icon = icon("play"))
    } else {
      animate(TRUE)
      updateActionButton(session, "playButton", label = "Pause", icon = icon("pause"))
    }
  })
  
  observeEvent(input$stopButton, {
    animate(FALSE)
    updateActionButton(session, "playButton", label = "Play", icon = icon("play"))
    updateSliderInput(session, "yearSlider", value = min(population_data$Year))
  })
  
  observe({
    if (animate()) {
      invalidateLater(2000, session)
      year <- input$yearSlider
      if (year < max(population_data$Year)) {
        updateSliderInput(session, "yearSlider", value = year + 1)
      } else {
        updateActionButton(session, "playButton", label = "Play", icon = icon("play"))
        animate(FALSE)
      }
    }
  })
  
  
  
}

shinyApp(ui, server)




