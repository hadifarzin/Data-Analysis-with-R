########################################
## Data Analysis with R course AMOD5250
## Date: 25 July 2021
## Developer: Hadi Farzin
## Email: hadifarzin@gmail.com
## View App link: https://6fzge0-h-farzin.shinyapps.io/Uber_Data_Analysis/
#######################################


library(shiny)
library(shinydashboard) # to create dashboard in shiny
library(ggplot2)
library(lubridate) # convert to Date and Time variable: to use functions month, day,year,hour
library(dplyr) # to use group_by function
library(leaflet) # to create map
library(leaflet.extras) # for using addHeatmap function

## Load dataset
uber <- read.csv("uber_small_dataset.csv")

## Important Note:  Due to high computational cost of loading the main dataset (with more than 3.5 milion records) I had to load a sample of the main dataset which contains 30,000 records that randomly sampled


# Convert values to Datetime
uber$Date.Time <- as.POSIXct(uber$Date.Time, format = "%m/%d/%Y %H:%M:%S")

# Create some new columns: Time
uber$Time <- format(as.POSIXct(uber$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

# Create some new columns: Month, Day, Weekday, Hour
uber$Month <- factor(month(uber$Date.Time, label = TRUE))
uber$Day <- factor(day(uber$Date.Time))
#uber$Year <- factor(year(uber$Date.Time))
uber$Weekday <- factor(wday(uber$Date.Time, label = TRUE))
uber$Hour <- factor(hour(hms(uber$Time)))

# define a color pallet for using in ggplot
colors2 = c("#CC1001", "#660555", "#05a099", "#cf0aca", "#f5a040", "#f683c9", "#ef75b0")

##################################
###  General Plots  #############


## 1. hourly 

## group by weekday and hour
uber_group_hour_DOW <- uber %>%
    group_by(Weekday) %>%
    dplyr::summarize(Total = n()) 

## Create plot: number of trips in different hours in weekdays
plot_hour_DOW_distribution <- ggplot(uber_group_hour_DOW, aes(Weekday, Total)) + 
    geom_bar( stat = "identity", fill = "purple", color = "red") +
    coord_flip() +
    theme_classic()


## 2. Monthly 

## group by month
uber_group_month <- uber %>%
    group_by(Month) %>%
    dplyr::summarize(Total = n()) 

## Create plot: number of trips in different month
plot_month_distribution <- ggplot(uber_group_month, aes(Month, Total)) + 
    geom_bar( stat = "identity", fill = "orange", color = "red") +
    theme_classic()


## 3. Daily

## group by month day
uber_group_day <- uber %>%
    group_by(Day) %>%
    dplyr::summarize(Total = n()) 

## Create plot: Total number of trips in each  day over the six month
plot_day_distribution <- ggplot(uber_group_day, aes(Day, Total)) + 
    geom_bar( stat = "identity", fill = "green", color = "black") +
    theme_classic() 


## 4.

## group by hour and month
uber_group_hour_month <- uber %>%
    group_by(Month,Hour) %>%
    dplyr::summarize(Total = n()) 

## Create plot: Total number of trips hourly in each month
plot_hour_month_distribution <- ggplot(uber_group_hour_month, aes(Hour, Total, fill = Month)) + 
    geom_bar( stat = "identity") +
    scale_fill_manual(values = colors2) +
    theme_classic()


## 5. Location

uber_rounded <- uber # a copy of data
uber_rounded$Lat <- round(uber$Lat,2) # round the Lat 
uber_rounded$Lon <- round(uber$Lon,2) # round the Lon 

# group by Lat and Lon
uber_group_lat <- uber_rounded %>%
  group_by(Lat,Lon) %>%
  dplyr::summarize(Total = n())

loc_max <- uber_group_lat # copy of data
loc_max <- loc_max[order(loc_max$Total, decreasing = TRUE),] # ordering data by number of trips in each location
loc_max_3 <- head(loc_max,3)[1:3,] # show the first 3 top locations with maximum trip 

# Showing on the map
location_most_popular <- leaflet() %>%
                          addTiles() %>%
                          setView(lat= loc_max_3$Lat[1], lng =  loc_max_3$Lon[1] , zoom=12) %>%
                          addCircleMarkers(lng = loc_max_3$Lon,lat=loc_max_3$Lat, radius = 10,color="blue")

##############################
######  UI and Server  #######
##############################


## Create UI
ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Uber Data Analysis"), ## Create a dashboard header
                    dashboardSidebar(width = 170,
                                     sidebarMenu( ## Creating sidebarMenu items
                                       
                                         br(),br(), ## make two empty lines
                                         
                                         menuItem("General Analysis", tabName = "General", icon = icon("chart-bar")),
                                         menuItem("Custom Analysis", tabName = "Custom", icon = icon("chart-line")),
                                         
                                         br(),br(),br(), ## make three empty lines
                                         
                                         menuItem("About this app", tabName = "help", icon = icon("info-circle")),
                                         menuItem("About Developer", tabName = "developer", icon = icon("user-secret")),
                                         
                                         br(),br(),br(),br(),br(),br(), ## make six empty lines
                                         
                                         div(imageOutput("my_image"), style="text-align: center;") ## load image Trent logo in sidebar
                                         #div(tags$img(src='trent_logo.png', height=50, width=150), style="text-align: center;") ## another way of loading image by html tag
                                        
                                         
                                     ) ## End of creating sidebarMenu items
                                     
                    ), ## Enf od creating a dashboard left slide bar
                    
                    dashboardBody(
                        fluidRow(#this fluidRow is for solving the main body height issue
                          
                            tabItems(
                              
                                ## Creating items for the first slidebar tab: General Analysis
                                tabItem("General",
                                        
                                        ## Create info boxes
                                        
                                        valueBox(subtitle = "City", value = "New York", color = "red", width=6, icon = icon("city")),
                                        valueBox(subtitle = "Total trrips in six month", value = "3.5M", color = "yellow", width=6, icon = icon("taxi")),
                                        infoBox(title = "Most crowded months", value = "September, August, July", width=6, icon = icon("calendar-plus"), color = "olive", fill = T),
                                        infoBox(title = "Most crowded month days", value = "10th, 12th, 30th", width=6, icon = icon("calendar-day"), color = "light-blue", fill = T),
                                        infoBox(title = "Most crowded weekday", value = "Thuesday, Friday", width=6, icon = icon("calendar-week"), color = "fuchsia", fill = T),
                                        infoBox(title = "Most crowded daily hour", value = "4-6 PM", width=6, icon = icon("hourglass-half"), color = "teal", fill = T),
                                        ######################################
                                        
                                        ## Create plot boxes
                                        
                                        box(title = "Number of Trips per Hour in a day", status = "primary", solidHeader = TRUE,
                                            plotOutput("distPlot0"),width = 6),
                                        
                                        box(title = "Total number of Trips per Month", status = "primary", solidHeader = TRUE,
                                            plotOutput("distPlot1"),width = 6),
                                        
                                        box(title = "Total number of Trips per Day in a Month", status = "primary", solidHeader = TRUE,
                                            plotOutput("distPlot2"),width = 6),
                                        
                                        box(title = "Number of Trips per Hour in a day per each month", status = "primary", solidHeader = TRUE,
                                            plotOutput("distPlot3"),width = 6),

                                        box(title = "Locations have the most demand for ride", status = "primary", solidHeader = TRUE,
                                            leafletOutput("distPlot4"),width = 12)                                                                                
                                        ######################################
                                        
                                ),## End of Creating dashboard body items for the first tab: General Analysis
                                
                                ## Creating items for the second tab in sidebar menu: Customized Analysis
                                tabItem("Custom",
                                        
                                        ## Create plot box
                                        box(title = "Trip distribution", status = "primary", solidHeader = TRUE,
                                            plotOutput("distPlot"),width = 9),
                                        
                                        ## Create input box
                                        box(title = "Input", status = "danger", solidHeader = TRUE, width = 3,height = 460,
                                            
                                            ## Select month
                                            selectInput("month",
                                                        "Select a month to analyse:",
                                                        choices = c("April" = "Apr", "May", "June" = "Jun", "July" = "Jul","August" = "Aug", "September" = "Sep")),
                                            
                                            ## Select type of analysis
                                            selectInput("type",
                                                        "Analysis by:",
                                                        choices = c("month day", "week day", "hour day")), 
                                            
                                            ## Select weekday if hourly selected in type of analysis
                                            radioButtons("wday",
                                                         "Select a week day",
                                                         choices = c("Saturday"="Sat","Sunday"="Sun", "Monday"="Mon", "Tuesday"="Tue", "Wednesday"="Wed", "Thursday"="Thu","Friday"="Fri"))
                                            
                                        ), ## End of creating input boxes
                                        
                                        ## Create map box
                                        box(title = "Trip distribution on map", status = "info", solidHeader = TRUE,
                                            leafletOutput("map2"),width=12)
                                        
                                ),## End of Creating dashboard body items for the second tab: Customized Analysis
                            
                                ## Creating item for the third tab in sidebar menu: About this app
                                tabItem("help", h4("This app was designed and implemented for the university course: Data Analysis with R.", br(), br(),"The purpose of this app is to provide essential information for Uber drivers to manage their time when they want to work.", br(),br(), "In this app drivers can understand when and where is the pick of the trip requests in different times in a year. What hour in a day, what day in a week, what day in a month and what month in a year")),

                                ## Creating item for the forth tab in sidebar menu: About Developer                                
                                tabItem("developer", h4(br(),br(),br(),br(),"--->", "Hadi Farzin", br(), "--->", "hadifarzin@trentu.ca", br(),br(), "--->", "Course: Data Analysis with R", br(),"--->", "Instructor: Dr. Albert Geoff Crane"))
                                
                            ) ## End of tabItems functions
                        )## End of fluidRow function
                    )## End of dashboardBody function
      )## End of dashboardPage function



## Create server 

server <- function(input, output) {
    
    output$distPlot <- renderPlot({
      
        ## Check the input 
        if (input$type == "month day"){
            uber_sampl2 <- filter(uber, Month ==input$month )
            
            uber_group_day <- uber_sampl2 %>%
                group_by(Day) %>%
                dplyr::summarize(Total = n())
            
            # plot number of trips in different hours in a day
            ggplot(uber_group_day, aes(Day, Total, group = 1)) + 
                geom_line() +
                geom_point() +
                ggtitle("By month day in:",input$month) + 
                theme_classic()
        } else if(input$type == "week day"){
            
            
            uber_sampl2 <- filter(uber, Month ==input$month)
            
            uber_group_weekday <- uber_sampl2 %>%
                group_by(Weekday) %>%
                dplyr::summarize(Total = n())
            
            # plot number of trips in different hours in a day
            ggplot(uber_group_weekday, aes(Weekday, Total, group = 1)) + 
                geom_line() +
                geom_point()  +
                ggtitle("By week day") + 
                theme_classic()
        } else {
            
            uber_sampl3 <- filter(uber, Month ==input$month & Weekday==input$wday  )
            
            uber_group_hour <- uber_sampl3 %>%
                group_by(Hour) %>%
                dplyr::summarize(Total = n())
            
            # plot number of trips in different hours in a day
            ggplot(uber_group_hour, aes(Hour, Total, group = 1)) + 
                geom_line() +
                geom_point() +
                ggtitle("Hourly in:", input$wday) + 
                theme_classic()
            
        }
    })
    output$map2 <- renderLeaflet({
        uber_sampl3 <- filter(uber, Month ==input$month & Weekday==input$wday  )
        leaflet() %>%
            addTiles() %>%
            setView(lat=40.669493, lng = -73.897354 , zoom=10) %>%
            addHeatmap(lng = uber_sampl3$Lon,lat=uber_sampl3$Lat,max = 1, radius = 8) 
    })
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(-93.65,42.05, zoom=17)
    }) 
    output$distPlot0 <- renderPlot({
        plot_hour_DOW_distribution
    })
    output$distPlot1 <- renderPlot({
        plot_month_distribution
    })
    output$distPlot2 <- renderPlot({
        plot_day_distribution
    })
    output$distPlot3 <- renderPlot({
        plot_hour_month_distribution
    })
    output$distPlot4 <- renderLeaflet({
      location_most_popular
    })    
    output$my_image <- renderImage({
        list(src = "trent_logo.png", height="50px", width="110px")
    },deleteFile=FALSE)
}


# Run the application 
shinyApp(ui = ui, server = server)
