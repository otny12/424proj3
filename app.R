#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
options("rgdal_show_exportToProj4_warnings"="none")
library(rgdal)
library(data.table)
#predefines
company_data <- read.delim(file="0.tsv", sep="\t", fileEncoding = "UTF-8")
# community_data <- read.delim(file="CommAreas.tsv", sep="\t", fileEncoding = "UTF-8")
#parse dates into column called date
company_data$date <- make_datetime(year= 2019
                           ,month = company_data$Trip.Start.Month
                           ,day = company_data$Trip.Start.Day
                           # ,hour = company_data$Trip.Start.Hour
)

geoj <-readOGR("Community.geojson")
geoj$data <- geoj$data[c(5),]
file_names <- paste0(c(0:54) ,".tsv")
# myfiles <- as.data.frame(lapply(file_names, read.delim))
# myfiles <- do.call(rbind, lapply(file_names, read.delim))
myfiles <- rbindlist(lapply(file_names, fread))
# fixed_names <- lapply(colnames(myfiles),make.names)
fixed_names <- c("Trip.Seconds", "Trip.Miles","Pickup.Community.Area","Dropoff.Community.Area","Company", "Trip.Start.Day","Trip.Start.Month", "Trip.Start.Hour")
colnames(myfiles) <- fixed_names
# temp<-rbindlist(myfiles)
# out_val <- lapply(myfiles$Dropoff.Community.Area, table)

# y <- sum(seq(as.Date("2019/1/1"), as.Date("2019/12/31"), "days") == ymd(c(company_data$date)))
# y <- aggregate(company_data, by=list(company_data$Trip.Start.Day, company_data$Trip.Start.Month),sum)
# y <- ymd(company_data$date)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Application title
    titlePanel(""),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(),

        # Show a plot of the generated distribution
        mainPanel(
          # verbatimTextOutput("test1"),
          plotOutput("bar_chart"),
          leafletOutput("Chicago"),
          radioButtons("to_from", "Select to see:",
                       choices = list("Inflow to" = "1", "Output to" = "2"),
                       selected = "1",
                       inline=TRUE),
        )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    com_areas <- reactive({
      drop_offs <- subset(myfiles, Pickup.Community.Area==strtoi(event()$id))$Dropoff.Community.Area
      drop_offs <- factor(drop_offs, levels=c(1:77))
      temp <- table(drop_offs)
      temp <- (temp/sum(temp))*100
      #get them in the right order cause data not ordered
      ordered_list <-c(1:77)
      reorder_index <- match(geoj@data$area_numbe, ordered_list)
      percentages_vec <- as.vector(temp)
      percentages_vec<-percentages_vec[reorder_index]
      geoj@data$value <- percentages_vec
      geoj
      })
    
    
    
    output$day_chart <- renderPlot({
      y <- table(company_data$date)
      x <- seq(as.Date("2019/1/1"), as.Date("2019/12/31"), "days")
      day_chart_data <- data.frame(x,y)
      ggplot(day_chart_data, aes(x=x, y=y)) + geom_bar(stat="identity", fill="steelblue") +
        labs(title = "Ridership from 2001 to 2021 by Day", x="Date", y = "Ridership") +
        scale_y_continuous(labels = comma)
    })
    
    output$hour_chart <- renderPlot({
      y <- table(company_data$Trip.Start.Hour)
      x <- c(0:23)
      hour_chart_data <- data.frame(x,y)
      ggplot(hour_chart_data, aes(x=x, y=y)) + geom_bar(stat="identity", fill="steelblue") +
        labs(title = "Ridership from 2001 to 2021 by Day", x="Date", y = "Ridership") +
        scale_y_continuous(labels = comma)
    })
    
    output$week_chart <- renderPlot({
      y <- table(wday(company_data$date))
      x <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday")
      weekday_chart_data <- data.frame(x,y)
      ggplot(weekday_chart_data, aes(x=x, y=y)) + geom_bar(stat="identity", fill="steelblue") +
        labs(title = "Ridership from 2001 to 2021 by Day", x="Date", y = "Ridership") +
        scale_y_continuous(labels = comma)
    })
    
    output$month_chart <- renderPlot({
      y <- table(month(company_data$Trip.Start.Month))
      x<- month.name[c(1:12)]
      month_chart_data <- data.frame(x,y)
      ggplot(month_chart_data, aes(x=x, y=y)) + geom_bar(stat="identity", fill="steelblue") +
        labs(title = "Ridership from 2001 to 2021 by Day", x="Date", y = "Ridership") +
        scale_y_continuous(labels = comma)
    })
    
    output$histo_miles_chart <- renderPlot({
      ggplot(company_data, aes(x=Trip.Miles)) + geom_dotplot()
      # max(company_data$Trip.Miles)
    })
    
    output$histo_time_chart <- renderPlot({
      ggplot(company_data, aes(x=Trip.Seconds)) + geom_dotplot()
    })
    
    output$Chicago <-renderLeaflet({
      # geojson <- readLines("Community.geojson", warn = FALSE) %>%
      #   paste(collapse = "\n")
      # leaflet() %>% addGeoJSON(geojson,
      #                          smoothFactor = 0.2,
      #                          fillOpacity = 1,
      #                          color = ~pal(temp)) %>% 
      #   addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      #   setView(lng=-87.63144, lat=41.88094,zoom=9)
      leaflet()%>% 
          addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
          setView(lng=-87.63144, lat=41.88094,zoom=9)
    })
    
    
    event <- reactive(input$Chicago_shape_click)
    
    observe({
      # #get dropoff community old way
      # drop_offs <- company_data$Dropoff.Community.Area
      # drop_offs <- factor(drop_offs, levels=c(1:77))
      # temp <- table(drop_offs)
      # printer <- as.vector(temp)
      # com_areas@data$value <- printer
      
      #get dropoff community based on event
      

      
      pal <- colorNumeric("viridis", NULL)
      
      leafletProxy("Chicago", data=com_areas()) %>%
        clearShapes() %>%
        addPolygons(fillOpacity = .5,
                    fillColor = ~pal(value),
                    layerId = ~area_numbe,
                    label = ~paste0(community," ",area_numbe)
                    )%>%
        removeControl(layerId = "legend")%>%
                    addLegend(pal = pal, values = ~value, opacity = 1.0,layerId = "legend",title="Percetage")
    })
    # output$test1 <- renderPrint(event())
    
}

# Run the application 
shinyApp(ui = ui, server = server)
