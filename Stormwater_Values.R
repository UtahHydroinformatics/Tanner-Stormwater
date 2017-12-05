# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(zoo)
library(WaterML)
library(plotly)
library(leaflet)

#Read Sites table for map locations

sites <- read.csv('./data/sites.csv', header = TRUE)
names(sites) <- c("site", "long", "lat")

# Define Pipe Dimensions, Slope, and Roughness Values

Slope <- 0.001
n <- 0.015

Gage <- 1.4

diameter <- (2/3.281)
radius <- (1/3.281)

# Link to EnviroDIY server

server <- "http://data.envirodiy.org/wofpy/soap/cuahsi_1_1/.wsdl"

# Get Judd Depth Sensor Data for Tanner Dance Building from EnviroDIY server 
Tanner_Distance <- GetValues(server, siteCode="envirodiy:Test_F", 
                          variableCode="envirodiy:Judd-Distance")

# Change Column Names and Convert Depth from Centimeters to Feet
Tanner_Distance <- Tanner_Distance[1:2]
names(Tanner_Distance) <- c("Date", "Distance")
Tanner_Distance <- subset(Tanner_Distance, Distance > 0)
Tanner_Distance$Distance <- Tanner_Distance$Distance/100
Tanner_Distance$Depth <- Gage-Tanner_Distance$Distance
Tanner_Distance <- subset(Tanner_Distance, Depth < diameter)
Tanner_Distance <- subset(Tanner_Distance, Depth > 0)

Tanner_Flow <- Tanner_Distance

Tanner_Distance <- Tanner_Distance[,c(1,3)]

names(Tanner_Distance) <- c('Date', 'DataValue')

Tanner_Distance_Download <- Tanner_Distance
names(Tanner_Distance_Download) <- c("Date", "Depth (m)")

Tanner_Distance$type <- "Depth (m)"

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

Tanner_Flow$Flow <- (1.00/n)*Tanner_Flow$Area*
  (Tanner_Flow$Hydraulic_Radius^(2/3))*(Slope^0.5)

Tanner_Flow <- Tanner_Flow[,c(1,8)]

names(Tanner_Flow) <- c('Date', 'DataValue')

Tanner_Flow_Download <- Tanner_Flow
names(Tanner_Flow_Download) <- c("Date", "Flow (cms)")

Tanner_Flow$type <- "Flow (cms)"


# Get Judd Temperature Sensor Data for Tanner Dance Building from EnviroDIY server 
Tanner_Temp <- GetValues(server, siteCode="envirodiy:Test_F", 
                         variableCode="envirodiy:Judd-Temperature")

Tanner_Temp <- Tanner_Temp[1:2]
names(Tanner_Temp) <- c("Date", "DataValue")

Tanner_Temp_Download <- Tanner_Temp
names(Tanner_Temp_Download) <- c("Date", "Judd_Temperature (°C)")

Tanner_Temp$type <- "Judd Air Temperature Sensor (°C)"

# Get Mayfly Temperature Sensor Data for Tanner Dance Building from EnviroDIY server 
Tanner_Mayfly_Temp <- GetValues(server, siteCode="envirodiy:Test_F", 
                         variableCode="envirodiy:EnviroDIY_Mayfly_Temp")

Tanner_Mayfly_Temp <- Tanner_Mayfly_Temp[1:2]
names(Tanner_Mayfly_Temp) <- c("Date", "DataValue")

Tanner_Temp_Mayfly_Download <- Tanner_Mayfly_Temp
names(Tanner_Temp_Mayfly_Download) <- c("Date", "Mayfly_Temperature (°C)")

Tanner_Mayfly_Temp$type <- "Mayfly Air Temperature Sensor (°C)"

Tanner_Storm <- rbind(Tanner_Flow, Tanner_Temp, Tanner_Mayfly_Temp, Tanner_Distance)

Tanner_Storm$Date <- Tanner_Storm$Date - (6*3600)

Tanner_Storm$site <- "Tanner Dance"

UU_Storm <- merge(Tanner_Storm, sites, by="site")

#download_data <- cbind.fill(Tanner_Flow_Download[1:2], Tanner_Temp_Download[2], Tanner_Temp_Mayfly_Download[2], Tanner_Distance_Download[2])

lastDate <- tail(UU_Storm$Date, n=2)

SeventyTwoBefore <- lastDate - (72*3600)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel(tags$a(href = "https://facilities.utah.edu/", "University of Utah Storm Drain Network", target = "_blank")),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select site location to plot
                    selectInput(inputId = "site", label = strong("Site"),
                                choices = unique(UU_Storm$site),
                                selected = "Tanner Dance"),
                    
                    # Select first data type to plot
                    selectInput(inputId = "type", label = strong("Primary Variable"),
                                choices = unique(UU_Storm$type),
                                selected = "Flow"),
                    
                    # Select second data type to plot
                    selectInput(inputId = "variable", label = strong("Secondary Variable"),
                                choices = unique(UU_Storm$type),
                                selected = "Judd Air Temperature Sensor (°C)"),
                    
                    # Select date range to be plotted
                    dateRangeInput("Date", strong("Date range"), start = SeventyTwoBefore, end = lastDate,
                                   min = "2017-01-01", max = "2020-12-31"),
                    
                    # Link to Github page
                    
                    tags$a(href = "https://github.com/UtahHydroinformatics/Tanner-Stormwater", "Shiny Code", target = "_blank"),
                    
                    # Button
                    downloadButton("downloadData", "Download Flow Rate Data")
                  ),
                   
                  
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotlyOutput(outputId = "lineplot", height = "600px"),
                    plotlyOutput(outputId = "secondplot", height = "600px"),
                    textOutput(outputId = "desc"),
                    tags$a(href = "http://data.envirodiy.org/", "Source: EnviroDIY", target = "_blank"),
                    leafletOutput("sensormap")
                  )
                )
                

)

# Define server function
server2 <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(input$site)
    req(input$type)
    req(input$Date)
    validate(need(!is.na(input$Date[1]) & !is.na(input$Date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$Date[1] < input$Date[2], "Error: Start date should be earlier than end date."))
    Tanner_Storm %>%
      filter(
        site == input$site,
        type == input$type,
        Date > as.POSIXct(input$Date[1]) & Date < as.POSIXct(input$Date[2]
        ))
  })
  
  second_data <- reactive({
    req(input$site)
    req(input$variable)
    req(input$Date)
    validate(need(!is.na(input$Date[1]) & !is.na(input$Date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$Date[1] < input$Date[2], "Error: Start date should be earlier than end date."))
    Tanner_Storm %>%
      filter(
        site == input$site,
        type == input$variable,
        Date > as.POSIXct(input$Date[1]) & Date < as.POSIXct(input$Date[2]
        ))
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlotly({
    p <- plot_ly(source = "source") %>% 
      add_lines(data = selected_trends(), x = ~Date, y = ~DataValue, mode = "lines", color = input$type, colors = c("#132B43", "#56B1F7")) %>%  
      add_trace(data = selected_trends(), x = ~Date, y = ~DataValue, mode = "markers", color= "Individual Reading") %>%
      layout(xaxis = list(title = "Date/Time (MTN Time)"), yaxis= list(title=input$type))
    p
    })
  
  output$secondplot <- renderPlotly({
    sp <- plot_ly(source = "source") %>% 
      add_lines(data = second_data(), x = ~Date, y = ~DataValue, mode = "lines", color = input$variable, colors = c("#132B43", "#56B1F7")) %>%  
      add_trace(data = second_data(), x = ~Date, y = ~DataValue, mode = "markers", color= "Individual Reading") %>%
      layout(xaxis = list(title = "Date/Time (MTN Time"), yaxis= list(title=input$variable))
    sp
  })
  
  #Add Map of sensor location
  output$sensormap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    locations <- subset(UU_Storm, site==input$site)
    leaflet() %>% 
      addTiles() %>%
      addMarkers(lng=locations$long, lat=locations$lat, label=input$site)
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      "Discharge_Data.csv"
    },
    content = function(file) {
      write.csv(Tanner_Flow_Download, file=file, row.names = FALSE)
    }
  )
  
}

# Create Shiny object
shinyApp(ui = ui, server = server2)

