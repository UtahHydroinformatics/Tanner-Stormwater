# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(zoo)
library(WaterML)
library(plotly)
library(leaflet)

# Link to EnviroDIY server

server <- "http://data.envirodiy.org/wofpy/soap/cuahsi_1_1/.wsdl"

# Get Judd Depth Sensor Data for Tanner Dance Building from EnviroDIY server 
Tanner_Flow2 <- GetValues(server, siteCode="envirodiy:STRM_01", 
                          variableCode="envirodiy:Judd-Distance")

# Change Column Names and Convert Depth from Centimeters to Feet
Tanner_Flow <- Tanner_Flow2[1:2]
names(Tanner_Flow) <- c("Date", "Distance")
Tanner_Flow <- subset(Tanner_Flow, Distance > 0)
Tanner_Flow$Distance <- Tanner_Flow$Distance*(39.37/1200)
Tanner_Flow$Depth <- 2-Tanner_Flow$Distance
Tanner_Flow <- subset(Tanner_Flow, Depth > 0)


# Define Pipe Dimensions, Slope, and Roughness Values

Slope <- 0.001
n <- 0.015

diameter <- 2
radius <- 1

# Calculate Theta based on Sensor Depth, Radius and Diameter

Tanner_Flow$Theta <- ifelse((Tanner_Flow$Depth < radius), pi - (2*asin(1-(2*Tanner_Flow$Depth/diameter))),  
                            pi + (2*asin((2*Tanner_Flow$Depth/diameter)-1)))


# Calculate Area from Theta and Diameter

Tanner_Flow$Area <- ((Tanner_Flow$Theta -sin(Tanner_Flow$Theta))*diameter^2)/8

# Calculate Wetted Perimeter from Theta and Diameter

Tanner_Flow$Wetted_Perimeter <- (Tanner_Flow$Theta*diameter)/2

# Calculate Hydraulic Radius

Tanner_Flow$Hydraulic_Radius <- Tanner_Flow$Area/Tanner_Flow$Wetted_Perimeter

# Calculate Flow using Manning's Equation

Tanner_Flow$Flow <- (1.49/n)*Tanner_Flow$Area*
  (Tanner_Flow$Hydraulic_Radius^(2/3))*(Slope^0.5)

Tanner_Flow <- Tanner_Flow[,c(1,8)]

names(Tanner_Flow) <- c('Date', 'DataValue')

Tanner_Flow$dt <- as.POSIXct(Tanner_Flow$Date,format= "%Y-%m-%d %H:%M:%S")


Tanner_Flow$type <- "Flow (cfs)"


# Get Judd Temperature Sensor Data for Tanner Dance Building from EnviroDIY server 
Tanner_Temp <- GetValues(server, siteCode="envirodiy:STRM_01", 
                         variableCode="envirodiy:Judd-Temperature")

Tanner_Temp <- Tanner_Temp[1:2]
names(Tanner_Temp) <- c("Date", "DataValue")

Tanner_Temp$dt <- as.POSIXct(Tanner_Temp$Date,format= "%Y-%m-%d %H:%M:%S")

Tanner_Temp$type <- "Temperature (°C)"

Tanner_Storm <- rbind(Tanner_Flow, Tanner_Temp)

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Tanner Building Storm Drain"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "type", label = strong("Trend index"),
                                choices = unique(Tanner_Storm$type),
                                selected = "Flow"),
                    
                    # Select date range to be plotted
                    dateRangeInput("Date", strong("Date range"), start = "", end = "",
                                   min = "2017-01-01", max = "2017-12-31")
                    
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotlyOutput(outputId = "lineplot", height = "600px"),
                    textOutput(outputId = "desc"),
                    tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: data.enviroydiy", target = "_blank"),
                    leafletOutput("sensormap")
                  )
                )
)

# Define server function
server2 <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(input$Date)
    validate(need(!is.na(input$Date[1]) & !is.na(input$Date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$Date[1] < input$Date[2], "Error: Start date should be earlier than end date."))
    Tanner_Storm %>%
      filter(
        type == input$type,
        Date > as.POSIXct(input$Date[1]) & Date < as.POSIXct(input$Date[2]
        ))
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlotly({
    p <- plot_ly(source = "source") %>% 
      add_lines(data = selected_trends(), x = ~Date, y = ~DataValue, mode = "lines", color = input$type, colors = c("#132B43", "#56B1F7")) %>%  
      add_trace(data = selected_trends(), x = ~Date, y = ~DataValue, mode = "markers", color= "Individual Reading")
    p
    })
  
  #Add Map of sensor location
  output$sensormap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% 
      addTiles() %>%
      addMarkers(lng=-111.841, lat=40.7624, label="HPER Mall Storm Drain")
  })
  

}

# Create Shiny object
shinyApp(ui = ui, server = server2)
