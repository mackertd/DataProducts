# Developing Data Products Shiny Project

library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)
library(ggplot2)
library(shinydashboard)

# Load the Dam Data

fileData <- readOGR(dsn = "GRanD_dams_v1_1.shp", "GRanD_dams_v1_1")

# Extract the attributes from the shape file

fileAttributes <- fileData@data

# Group By Country

byCountry <- group_by(fileAttributes, COUNTRY)

# Summarise the data

damPlotSummary <- summarise(byCountry, count = n())

damSummary <- summarise(damPlotSummary, mean(count), median(count), min(count), max(count))

colnames(damSummary) <- c("Mean", "Median", "Min", "Max")

# Create the UI

ui <- dashboardPage(
      
      dashboardHeader( title = "Dam Data Explorer"),
      
      dashboardSidebar(
            
            sidebarMenu(
                  menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
                  menuItem("Data Explorer", tabName = "data", icon = icon("dashboard")),
                  menuItem("Geospatial Display", tabName = "geospatial", icon = icon("th")),
                  menuItem("Documentation", tabName = "documentation", icon = icon("th"))
            )
            
      ),
      
      dashboardBody(
            
            tabItems(
                  
            
                  tabItem(tabName = "dashboard",
                          
                          h2("GRanD Database Overview", style = "text-align: center"),
                          includeMarkdown("overview.Rmd")
                          
                  ),
                  
                  tabItem(tabName = "data", 
                        
                        h2("Number of Dams By Country", style = "text-align: center"),
                        
                              fluidRow(
                                    
                                    valueBoxOutput("data1Box", width = 3),
                                    valueBoxOutput("data2Box", width = 3),
                                    valueBoxOutput("data3Box", width = 3),
                                    valueBoxOutput("data4Box", width = 3)
                                    
                              ),
                              
                              fluidRow(
                          
                
                                    box(plotOutput("plot1"), height=480, width=12)
                              
                              ),
                        
                              fluidRow(
                        
                                    box(sliderInput("slider1", "Number of Dams", min = damSummary$Min, max = damSummary$Max,
                                                    value = c(damSummary$Min, damSummary$Max)), width = 12, style = "text-align: center")
                                    
                              )
                              
                  ),
                  
                  tabItem(tabName = "geospatial", 
                          
                          h2("Geospatial Display", style = "text-align: center"),
                          leafletOutput("map"),
                          
                          actionButton("plot", label = "Press To Plot Dams On The Map")
                          
                  ),
                  
                  
                  tabItem(tabName = "documentation",
                          
                          h2("Documentation", style = "text-align: center"),
                          includeMarkdown("Documentation.Rmd")
                          
                  )
            
            )
                    
      )

)

# Create the Server

server <- function(input, output) {
      
      # Blank map on the Map Page
      
      output$map <- renderLeaflet({
            leaflet() %>% setView(lng = -98.583, lat = 39.833, zoom = 3) %>%
                  addProviderTiles("Stamen.TonerLite",
                                   options = providerTileOptions(noWrap = TRUE)
                  ) 
      })
      
      # Button to plot the dams on the map
      
      observeEvent(input$plot, {
      
         output$map <- renderLeaflet({
                  leaflet(fileData) %>% setView(lng = -98.583, lat = 39.833, zoom = 3) %>%
                        addProviderTiles("Stamen.TonerLite",
                                         options = providerTileOptions(noWrap = TRUE)
                        )  %>% addMarkers(clusterOptions = markerClusterOptions(), popup = ~as.character(paste("<b>Dam Name</b><br>",DAM_NAME)))
               
            })
            
      })

      
      # Value Boxes
      
      output$data1Box <- renderValueBox({
            
            valueBox(
                  input$slider1[1], "Min Number of Dams Shown", icon = icon("th-large", lib = "glyphicon"),
                  color = "aqua"
            )

      })
      
      output$data2Box <- renderValueBox({
            
            valueBox(
                  input$slider1[2], "Max Number of Dams Shown", icon = icon("th-large", lib = "glyphicon"),
                  color = "aqua"
            )
            
      })
      
      output$data3Box <- renderValueBox({
            
            minVal <- input$slider1[1]
            maxVal <- input$slider1[2]
            
            damPlotSummary <- filter(damPlotSummary, count >= minVal, count <= maxVal)
            
            damSummary <- summarise(damPlotSummary, mean(count), median(count), min(count), max(count))
            
            colnames(damSummary) <- c("Mean", "Median", "Min", "Max")
            
            valueBox(
                  round(damSummary$Mean, digits = 2), "Mean Number of Dams Shown", icon = icon("th-large", lib = "glyphicon"),
                  color = "aqua"
            )
            
      })
      
      output$data4Box <- renderValueBox({
            
            minVal <- input$slider1[1]
            maxVal <- input$slider1[2]
            
            damPlotSummary <- filter(damPlotSummary, count >= minVal, count <= maxVal)
            
            damSummary <- summarise(damPlotSummary, mean(count), median(count), min(count), max(count))
            
            colnames(damSummary) <- c("Mean", "Median", "Min", "Max")
            
            valueBox(
                  round(damSummary$Median, digits = 2), "Median Number of Dams Shown", icon = icon("th-large", lib = "glyphicon"),
                  color = "aqua"
            )
            
      })
      
      # Plot for the initial bar chart
      
      output$plot1 <- renderPlot({
            
            
            minVal <- input$slider1[1]
            maxVal <- input$slider1[2]
            
            damPlotSummary <- filter(damPlotSummary, count >= minVal, count <= maxVal)
            
            
            plotBar <- ggplot(data=damPlotSummary, aes(x=COUNTRY, y=count)) +
                  geom_bar(stat="identity", fill="steelblue") +
                  geom_text(aes(label=count), vjust=-0.3, size=3.5) +
                  labs(title="Dams By County", x="Country", y = "Count") +
                  theme(axis.text.x=element_text(angle=90, hjust=1, color = "black"))
            
            print(plotBar)
            
      })

}

shinyApp(ui = ui, server = server)