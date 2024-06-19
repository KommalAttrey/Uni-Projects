#install.packages("shiny")
#install.packages("leaflet")
#install.packages("ggplot2")

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)


#calculation part
#################################################################

#reading data
data <- read.csv("Victoria_Accident_Data_FIT5147S12024PE2v2.csv")
#counting the number of accident based upon the combination of speed zone and light condition
agg_data <- aggregate(ACCIDENT_NO ~ SPEED_ZONE + LIGHT_CONDITION_DESC, data = data, FUN = length)
# creating ACCIDENT_HOUR column in dataframe by calculating hour using as.POSIXlt function
data$ACCIDENT_HOUR <- as.POSIXlt(data$ACCIDENT_TIME, format="%H:%M:%S")$hour
#using pipe operator 
#grouping the data based upon SPEED_ZONE, ACCIDENT_HOUR and counting the number of accident each hour
agg_data_hour <- data %>%
  group_by(SPEED_ZONE, ACCIDENT_HOUR) %>%
  summarise(Num_Accidents = n())

#################################################################


# UI
ui <- fixedPage(
  #title
  titlePanel(div("Vehicle Road Crash Data Exploration", style = "text-align: center;")),
  # Vis1 and Vis2
  fixedRow(
    column(width = 6, plotOutput("vis1")),
    column(width = 6, plotOutput("vis2")),
    column(width = 6, verbatimTextOutput("vis1_desc")),
    column(width = 6, verbatimTextOutput("vis2_desc"))
  ),
  
  # Map and map description
  fixedRow(
    column(width = 12, leafletOutput("map")),
    #adding checklist
    column(width = 12, checkboxGroupInput("daynight_filter", "Filter by Daynight", 
                                          choices = c("Day", "Dusk/Dawn", "Night"), 
                                          inline = TRUE)),
    column(width = 12, verbatimTextOutput("map_desc")),
    #adding range slider
    column(width = 12, sliderInput("severity_filter", "Filter by Severity Rank", 
                                   min = min(data$SEVERITY_RANK), 
                                   max = max(data$SEVERITY_RANK), 
                                   value = c(min(data$SEVERITY_RANK), max(data$SEVERITY_RANK)), 
                                   step = 1))
  ),
  
  #Data source
  fixedRow(
    column(width = 12, div("Data Source: Victoria_Accident_Data_FIT5147S12024PE2v2.csv",style = "text-align: center;"))
  ),
  
)

#################################################################

# Server
server <- function(input, output) {
# Vis1 plot
  output$vis1 <- renderPlot({
    ggplot(data, aes(x = SPEED_ZONE, fill = LIGHT_CONDITION_DESC)) +
      geom_bar() +
      labs(title = "Number of Accidents by Light Condition and Speed Zone",
           x = "Speed Zone",
           y = "Number of Accidents") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  # Vis1 plot description
output$vis1_desc <- renderText({
    "x-axis represents 'Speed Zone' and y-axis represents 'Number of accidents'
Vis1 shows the maximum number of accidents that occurred during the 'Day'when 'speed_zone'(60-100). 
Additionally,  when 'speed_zone' was 100 and Light condition is 'Dark Street lights on' number of accidents were large as compared to 'lower speed zones'.
Furthermore, there are more accidents observed under the 'Dusk/Dawn' light condition."
  })
  
 
###################################

# Vis2 plot
  output$vis2 <- renderPlot({
    
    # Filtering 4 top speed zones according to vis1
    top_speed_zones <- agg_data %>%
      arrange(desc(ACCIDENT_NO)) %>%
      slice(1:4) %>%
      pull(SPEED_ZONE)
    filtered_data <- filter(agg_data_hour, SPEED_ZONE %in% top_speed_zones)
    
    
    # Plot the data
    ggplot(filtered_data, aes(x = ACCIDENT_HOUR, y = Num_Accidents, color = as.factor(SPEED_ZONE))) +
      geom_line() +
      labs(title = "Number of Accidents by Hour and Speed Zone",
           x = "Hour of the Day",
           y = "Number of Accidents",
           color = "Speed Zone") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
# Vis2 plot description
  output$vis2_desc <- renderText({
"x-axis represents 'Hour of the Day' and y-axis represents 'Number of accidents' and 'Speed zone 'is represented by colours
Vis2 shows, that the highest number of accidents occurs during daytime hours,(between 6 AM and 5 PM).The top 4 speed zones according to vis1 was 100,80,60,50
Moreover, accidents peak when the speed zone is 100 between 12 PM and 6 PM .Minimum number of accident occurs when the speed zone is 50 as compaared to others.
Also 0-5 and 18-24 are the quiet hours of the day with less number of accidents."
  })
  
################################### 
 
#Map 
  #filtering data based on the selected option from the checklist
  filtered_data <- reactive({
    filtered <- data
    #Filter the dataset based on the selected day/night options and include night conditions if "Night" is selected
    if (!is.null(input$daynight_filter)) {
      if ("Night" %in% input$daynight_filter) {
        night_conditions <- c("Dark No street lights", "Dark Street lights on", "Dark Street lights unknown", "Dark Street lights off")
        filtered <- filtered[filtered$LIGHT_CONDITION_DESC %in% c(input$daynight_filter, night_conditions), ]
      } else {
        filtered <- filtered[filtered$LIGHT_CONDITION_DESC %in% input$daynight_filter, ]
      }
    }
    if (!is.null(input$severity_filter)) {
      filtered <- filtered[filtered$SEVERITY_RANK >= input$severity_filter[1] & filtered$SEVERITY_RANK <= input$severity_filter[2], ]
    }
    return(filtered)
  })
  
  
  output$map <- renderLeaflet({
    leaflet(data = filtered_data()) %>%
      setView(lng = 145.465783, lat = -38.482461, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addTiles() %>%
      addCircleMarkers(
        color = ~ifelse(LIGHT_CONDITION_DESC == "Day", "blue",
                        ifelse(LIGHT_CONDITION_DESC == "Dusk/Dawn", "orange", "red")),
        radius = ~10 / SEVERITY_RANK,  # Scale and reverse 
        opacity = 0.6,
        group = "accidents",
        popup = ~paste("Accident Date-:", ACCIDENT_DATE,", ",
                       "Accident Type-:", ACCIDENT_TYPE_DESC,", ",
                       "Light Condition-:", LIGHT_CONDITION_DESC,", ",
                       "Road Geometry-:", ROAD_GEOMETRY_DESC,", ",
                       "Speed Zone-:", SPEED_ZONE)
      ) %>%
      addLegend(position = "bottomright", 
                colors = c("blue", "orange", "red"),
                labels = c("Day", "Dusk/Dawn", "Night"),
                title = "Light Condition")
  })
  
# Description for Map
  output$map_desc <- renderText({
"This map displays the locations of accidents, with circular markers representing each accident.
The color of the markers indicates the light condition during the accident, with blue representing 'Day', orange representing 'Dusk/Dawn', and red representing 'Night' (other light conditions.)
Maximum number of accidents occur when Light Condition is 'Day'
Minimum number of accidents ocuur when Light Condition is 'Dusk/Dawn'
It seems like maximum number of accidents occur near the area 'Inverloch' and 'Philip Island road'
Most serve accidents are shown by larger circles and it seems that most served accident (rank1) mostly occured when light condition is day"
  })
}

###################################

# Run
shinyApp(ui = ui, server = server)





#References and citations

#https://search.r-project.org/CRAN/refmans/shiny/html/fixedPage.html
#https://rdrr.io/github/rstudio/shiny/man/checkboxInput.html
#I am submitting it second time at first time i forgot to did some implementation in map:)




