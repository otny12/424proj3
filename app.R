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
# company_data <- read.delim(file="0.tsv", sep="\t", fileEncoding = "UTF-8")
# community_data <- read.delim(file="CommAreas.tsv", sep="\t", fileEncoding = "UTF-8")
#parse dates into column called date
# company_data$date <- make_datetime(year= 2019
#                            ,month = company_data$Trip.Start.Month
#                            ,day = company_data$Trip.Start.Day
#                            # ,hour = company_data$Trip.Start.Hour
# )

geoj <-readOGR("Community.geojson")
geoj$data <- geoj$data[c(5),]
file_names <- paste0(c(0:54) ,".tsv")
# myfiles <- as.data.frame(lapply(file_names, read.delim))
# myfiles <- do.call(rbind, lapply(file_names, read.delim))
myfiles <- rbindlist(lapply(file_names, fread))
# fixed_names <- lapply(colnames(myfiles),make.names)
fixed_names <- c("Trip.Seconds", "Trip.Miles","Pickup.Community.Area","Dropoff.Community.Area","Company", "Trip.Start.Day","Trip.Start.Month", "Trip.Start.Hour")
colnames(myfiles) <- fixed_names
companies <- read.delim("Companies.tsv")

ui <- fillPage(
  fillRow(
    fillCol(plotOutput("map_chart",height ="100%"),
            actionButton("show", "About"),
            flex = c(95,5)
            ),
  fillCol(leafletOutput("Chicago", height = "100%")
          ),
  fillCol(hr(),
    radioButtons("to_from", "Select to see:",
                                      choices = list("Inflow to" = "1", "Output to" = "2"),
                                      selected = "1",
                                      inline=TRUE
                       ),
          radioButtons("miles", "Units",
                       choices = list("Miles" = "1",
                                      "Km" = "2")
                       ),
          textOutput("text"),
          flex = c(85,5,5,5)
          ),
  fillCol(plotOutput("day_chart"),
          plotOutput("hour_chart"),
          plotOutput("week_chart")
          ),
  fillCol(plotOutput("month_chart"),
          plotOutput("histo_miles_chart"),
          plotOutput("histo_time_chart"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    com_areas <- reactive({
      #update inflow or outflow
      drop_offs <- switch(input$to_from,
             "1" = subset(myfiles, Pickup.Community.Area==strtoi(event()$id))$Dropoff.Community.Area,
             "2" = subset(myfiles, Dropoff.Community.Area==strtoi(event()$id))$Pickup.Community.Area
      )
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
    
    
    
    company_data <- reactive({
      small_set <- switch(input$to_from,
             "1" = subset(myfiles, Pickup.Community.Area==strtoi(event()$id)),
             "2" = subset(myfiles, Dropoff.Community.Area==strtoi(event()$id))
      )
               
      date <- make_date(
        year= 2019
                                         ,month = small_set$Trip.Start.Month
                                         ,day = small_set$Trip.Start.Day
                            )
      data.frame(date,small_set$Trip.Start.Hour,small_set$Trip.Miles,small_set$Trip.Seconds)
    })
    
    
    
    output$day_chart <- renderPlot({
      x <- data.table( as.character(seq(ymd("2019/1/1"), ymd("2019/12/31"), "1 day")))
      colnames(x)<-"y"
      y<-company_data()$date
      # y<-factor(y, levels=x)
      y <- data.table(table(y))
      y <- merge(x,y,all.x=TRUE)
      # day_chart_data <- data.frame(x,y)
      y$y <- ymd(y$y)
      ggplot(y, aes(x=y, y=N)) + geom_bar(stat="identity", fill="steelblue") +
        labs(title = "Ridership For the Year of 2019", x="Date", y = "Ridership")+
        theme(axis.text.x = element_text(angle=45))+
        scale_x_date(labels = date_format("%m-%d-%Y"), date_breaks = "30 days")
    })
    
    output$hour_chart <- renderPlot({
      y <- table(company_data()$small_set.Trip.Start.Hour)
      x <- c(0:23)
      hour_chart_data <- data.frame(x,y)
      ggplot(hour_chart_data, aes(x=x, y=y)) + geom_bar(stat="identity", fill="steelblue") +
        labs(title = "Ridership For the Year of 2019 by Hour", x="Hour", y = "Ridership") +
        scale_y_continuous(labels = comma)
    })

    output$week_chart <- renderPlot({
      y <- table(wday(company_data()$date))
      x <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday")
      weekday_chart_data <- data.frame(x,y)
      ggplot(weekday_chart_data, aes(x=factor(x,weekdays(as.Date('1970-01-03')+1:7)), y=y)) + geom_bar(stat="identity", fill="steelblue") +
        labs(title = "Ridership For the Year of 2019 by Day of the Week", x="Day", y = "Ridership") +
        scale_y_continuous(labels = comma)
    })

    output$month_chart <- renderPlot({
      y <- table(month(company_data()$date))
      x<- month.name[c(1:12)]
      month_chart_data <- data.frame(x,y)
      ggplot(month_chart_data, aes(x=factor(x, levels=month.name), y=y)) + geom_bar(stat="identity", fill="steelblue") +
        labs(title = "Ridership for the Year of 2019 by Month", x="Month", y = "Ridership") +
        scale_y_continuous(labels = comma)
    })

    output$histo_miles_chart <- renderPlot({
      x <- switch(input$miles,
                  "1" = company_data()$small_set.Trip.Miles,
                  "2" = 1.60934*(company_data()$small_set.Trip.Miles)
      )
      ggplot(company_data(), aes(x=x)) + geom_histogram(colour="steelblue",fill="white",bins=20) +
        labs(title = "Distances traveled for the Year of 2019", x="Distance", y = "Count")+
        scale_y_continuous(labels = comma)
      
      # max(company_data$Trip.Miles)
    })

    output$histo_time_chart <- renderPlot({
      ggplot(company_data(), aes(x=small_set.Trip.Seconds)) + geom_histogram(colour="steelblue",fill="white",bins=20) +
        labs(title = "Duration of Trips traveled for the Year of 2019", x="Duration", y = "Count")+        
        scale_y_continuous(labels = comma)

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
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomleft' }).addTo(this)
    }")%>%
          setView(lng=-87.63144, lat=41.88094,zoom=12)
    })
    
    
    event <- reactive(input$Chicago_shape_click)
    output$text <- renderText({
      x <- which(geoj@data[["area_num_1"]]==strtoi(event()$id))
      paste("Now viewing: ",geoj@data$community[x])
    })
    
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
    
    output$map_chart <- renderPlot({
      x <- com_areas()@data$community
      y <- com_areas()@data$value
      plot <- data.frame(x,y)
      ggplot(plot, aes(x=x,y=y)) +
        geom_bar(stat="identity", fill="steelblue") +
        theme(axis.text.x = element_text(angle=45)) +
        coord_flip()
    })
    observeEvent(input$show, {
      showModal(modalDialog(
        title = "About",
        "This visualization was done by Tony Lau, using data provided by the Chicago Data protal. Data is available for download at 
      https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy
      Date of publication is 4/23/2022.  This was created as Project 3 of CS 424 taught by Proffessor Johnson.  This app is designed to be run on a 5760x1620 wall display, to view best in browser zoom a 1920x1080 display to 50%"
      ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
