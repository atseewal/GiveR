#############################
##                         ##
##          GiveR          ##
##      Shiny Web App      ##
##                         ##
##       By: DS, TW        ##
##                         ##
#############################

# Libraries ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(googlesheets)
library(htmltools)
library(leaflet)
library(maps)
library(DT)


# Data --------------------------------------------------------------------

percentServed <- data.frame(
    month = factor(x = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")),
    percent = c(8.11, 6.56, 7.97, 7.14, 6.00, 8.21, 7.49, 8.84, 8.40, 9.52, 11.23, 10.52)
)

povertyData <- read_csv("povertydata.csv")
countyNames <- read_csv("MichiganCountyNames.csv")
mapdata <- read_csv("mapdata.csv")
tableData <- read_csv("tableData.csv")

mich <- map_data("state", region="michigan")%>%filter(subregion=="south")

needLevels <- c("Low", "Medium", "High", "Critical")
needTypes <- c("Food", "Shelter", "Clothing", "Household", "Personal Care Items", "Funding", "Volunteers")
distanceLevels <- c(2, 5, 10, 25, 50)
cityNames <- c("City", "Names", "Here")

# getColor <- function(mapdata) {
#     sapply(mapdata$F_need, function(F_need) {
#         if(F_need == "Low") {
#             "green"
#         } else if(F_need == "Medium") {
#             "yellow"
#         } else if(F_need == "High") {
#             "orange"
#         } else {
#             "red"
#         } })
# }
# 
# icons <- awesomeIcons(
#     icon = 'fa-map-marker-alt',
#     iconColor = 'black',
#     library = 'fa',
#     markerColor = getColor(mapdata)
# )

m2 <- mapdata %>% leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>% addMarkers(lng = ~Lat, lat = ~Long, popup = mapdata$Org)

# Server ------------------------------------------------------------------

server <- function(input, output) {

    output$recType <- renderText({input$recType})
    output$recCounty <- renderText({input$recCounty})
    output$recCity <- renderText({input$recCity})
    output$recNeedLevel <- renderText({input$recNeedLevel})
    output$recDistance <- renderText({input$recDistance})
    output$giveType <- renderText({input$giveType})
    output$giveCounty <- renderText({input$giveCounty})
    output$giveCity <- renderText({input$giveCity})
    output$giveNeedLevel <- renderText({input$giveNeedLevel})
    
    output$g1 <- renderPlot({ggplot(aes(x = percentServed$month, y = percentServed$percent), data = percentServed) + geom_col(fill = "blue") + coord_flip() + labs(x = "Percent", y = "Month", caption = "Estimated need based on Open Hands Food Pantry serving rates from 2009-2010") + theme_bw()})
    
    output$g2 <- renderPlot({ggplot(povertyData%>%filter(Area == input$giveCounty), aes(x=Year, y=Percent))+geom_line(size = 3) + geom_point(aes(col=level), size = 5) + geom_hline(yintercept = 13.7) + labs(caption = "Source: US Census data") + theme_bw()})
    
    output$m1 <- renderPlot({ggplot() + geom_polygon(data = mich,  aes(long, lat, group = group), fill = "grey")+geom_point(data=mapdata, aes(x=Lat, y=Long, col=F_need),alpha=.7, size = 15)+labs(col="Need Level")})
    
    output$dattab <- renderDataTable(datatable(tableData, rownames = FALSE, filter = "top"))
    
    output$m2 <- renderLeaflet(m2)
    output$m3 <- renderLeaflet(m2)
    
}


# Header ------------------------------------------------------------------

header <- dashboardHeader(
    title = "MLH Assistance App",
    dropdownMenu(
        type = "tasks",
        taskItem(
            text = "Change the world, one step at a time", value = 90
        )
    )
)


# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            "Home", tabName = "home"
        ),
        menuItem(
            "Receiving", tabName = "receiving"
        ),
        menuItem(
            "Giving", tabName = "giving"
        )
    )
)


# Body --------------------------------------------------------------------

body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "home",
            h1("Home"),
            fluidRow(
                valueBox(value = "Featured:", subtitle = "Gleaner's Food Bank", color = "green"),
                valueBox(value = "14.9%", subtitle = "Percentage of Michigan in Poverty", color = "blue"),
                valueBox(value = "10.52%", subtitle = "Percent of People served in December", color = "red")
            ),
            fluidRow(
                box(width = 12, title = "State Organizations Map", plotOutput("m1"), footer = "Organizations in Michigan and Their Need")
            )
        ),
        tabItem(
            tabName = "receiving",
            h1("Receiving Page"),
            fluidRow(
                box(width = 12,
                    box(width = 3,
                        selectInput(
                            inputId = "recType",
                            "Type of Need",
                            needTypes
                        )),
                    box(
                        width = 3,
                        "Location",
                        selectInput("recCounty",
                                    "County",
                                    countyNames),
                        selectInput("recCity",
                                    "City",
                                    cityNames)
                    ),
                    box(width = 3,
                        selectInput(
                            "recNeedLevel",
                            "Need Level",
                            needLevels
                        )
                    ),
                    box(width = 3,
                        selectInput(
                            "recDistance",
                            "Distance",
                            distanceLevels
                        )
                    )
                )
            ),    
            fluidRow(
                box(width = 6, leafletOutput("m2")),
                box(width = 6, title = "List of Locations", dataTableOutput("dattab"))
            ),
            fluidRow(
                box(width = 12, "Government Programs")
            )
        ),
        tabItem(
            tabName = "giving",
            h1("Giving headquarters: Spread the Love"),
            fluidRow(
                box(
                    title = "Monthly Need Graph", width = 6,
                    plotOutput("g1")
                ),
                box(
                    title = "In Poverty County/Year Graph", width = 6,
                    plotOutput("g2")
                )
            ),
            fluidRow(
                box(
                    title = "Humanitarian Statement", width = 12,
                    h3("The goal of this application is to provide a space for individuals to have access to concise information on where they can receive the aid that they need. Further, we aim to provide a place for those wishing to help aid others a similar place to see where their time and efforts can be best spent. Lastly, organizations have the opportunity to update which particular needs they have at any given time")
                )
            ),
            fluidRow(
                box(width = 3,
                    selectInput(
                        "giveType",
                        "Type of Need",
                        needTypes
                    )
                ),
                box(width = 3,
                    selectInput(
                        "giveCounty",
                        "County",
                        countyNames
                    )
                ),
                box(width = 3,
                    selectInput(
                        "giveCity",
                        "City",
                        cityNames
                    )
                ),
                box(width = 3,
                    selectInput(
                        "giveNeedLevel",
                        "Need Level",
                        needLevels
                    )
                )
            ),
            fluidRow(
                box(title = "Big 'ol Box", width = 12,
                        HTML('<a href="https://docs.google.com/forms/d/e/1FAIpQLSfXJQyhRhWyqoMVrWND19EpHbN9WpNAE8h_LjMACFHe_AtSSw/viewform?vc=0&c=0&w=1"> I\'m an organization </a>'),
                    leafletOutput("m3")
                )
            )
        )
    )
)


# UI ----------------------------------------------------------------------

ui <- dashboardPage(header, sidebar, body)

shinyApp(ui, server)