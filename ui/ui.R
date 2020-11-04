#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages = c('rgdal', 'spdep', 'tmap', 'sf', 'ggpubr', 'cluster', 'factoextra', 'NbClust', 'heatmaply', 'corrplot', 'psych', 'tidyverse', 'shiny', 'shinydashboard', 'DT')
for (p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}

ui <- dashboardPage(
    dashboardHeader(
        tags$li(class = "dropdown",
                tags$style(".main-header {max-height: 100px;}"),
                tags$style(".main-header .logo {height: 80px;}"),
                tags$style(".btn.disabled {background-color: transparent;}"),
        ),
        title = div(actionButton("Overview", img(src="Landmarkdown_logo.png", width = 130, height=80), style="background-color: transparent; border-width: 0px"))),
    dashboardSidebar(
        tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
        sidebarMenu( id="sidebar",
            sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
            menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
            menuItem("Data", tabName = "Data", icon = icon("database")),
            menuItem("EDA", tabName = "EDA", icon = icon("chart-pie")),
            menuItem("SPP", tabName = "SPP", icon = icon("map-marker-alt")),
            menuItem("Clustering", tabName = "Clustering", icon = icon("object-group"))
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
            tabItem(tabName = "Overview",
                    fluidRow(
                        column(10, offset=1, box(width = NULL, status = "primary", style = "font-size: 18px;",
                            h1("Introduction", align="center"),
                            p("Over the years, planning for Singapore’s long term sustainability and optimising usage of our limited land have always been a 
                              strategic challenge for Singapore’s government agencies. The Urban Redevelopment Authority (URA) has been launching initiatives such
                              as the Concept Plan and Master Plan in an effort to develop Singapore’s physical environment in a sustainable manner. However, data 
                              shared by various agencies at public online data portals have not been explored or worked on extensively within their counterparts to 
                              gain a better understanding on the spatial distribution of businesses in Singapore and thus unable to more effectively translate them 
                              into better insights."),
                            p("Our team feels that we can contribute to this area by demonstrating how we can make use of data provided by these open data portals 
                              and analyse them together in order to showcase the role of geospatial analytics in Singapore.")
                            )
                            
                      )),
                    fluidRow( 
                        column(10, offset=1, box(width=NULL, status = "primary", style="font-size:18px",
                            h1("Project Objectives", align="center"),
                            p("For this project, we would be creating an analytical tool for observing and analysing the spatial point patterns of business entities in Singapore. Specifically, it focuses on the following objectives:"),
                            tags$li("To visualise the spatial distribution of different business types in Singapore on an internet-based map such as OpenStreetMap."),
                            tags$li("To conduct statistical simulations to reveal evidence of clusters using spatial point patterns analysis."),
                            tags$li("To conduct clustering to reveal characteristics of spatially constrained clusters."),
                            tags$li("To provide a user-friendly interface through the creation of a RShiny application to apply relevant filters for different business types."))
                    )),
                   fluidRow(
                       column(10, offset=1, box(width = NULL, status = "primary", align="center", 
                           h2("Navigate at the sidebar if you are interested in")
                    ))),
                   fluidRow(
                       column(4, offset=2, box( title = "Data", width = NULL, background = "red",
                               "This consist of the dataset used for our analysis. In the section, you will be able to see the raw data and what each columns
                                  signifies.")),
                       column(4, box( title = "EDA", width = NULL, background = "blue",
                                      "Exploratory Data Analysis. This is the stage where we try to understand the data with maps and charts"))),
                fluidRow(
                      column(4,offset=2, box( title = "SPP", width = NULL, background = "green",
                                      "Spatial Points Pattern. Visualise the areas that are more concentrated with selected industry.")),
                       column(4, box( title = "Clustering", width = NULL, background = "yellow",
                                      "Which are the few subzones that have similar industries in the area? How are they grouped together?")),
                   )
            ),
            
            # Second tab content
            tabItem(tabName = "Data",
                    fluidRow(
                        column(11, box(width=NULL, status = "primary", style="font-size:16px",
                            h1("Corporate Entities Information"),
                            p("This dataset comprises data on entities that are registered in Singapore, with additional information such as paid-up capital."),
                            DT::dataTableOutput("corp_info_table")
                        ))),
                    fluidRow(
                        column(11, box(width=NULL, status = "primary", style="font-size:16px",
                            h1("Singapore Standard Industrial Classification"),
                            p("The Singapore Standard Industrial Classification (SSIC) is the national standard for classifying economic activities undertaken by economic units."),
                            DT::dataTableOutput("ssic2020")
                        ))),
                    fluidRow(
                        column(11, box(width=NULL, status = "primary", style="font-size:16px",
                            h1("OneMap Address Data Search"),
                            p("This search API returns the geometry of an address based on search values such as postal codes."),
                            DT::dataTableOutput("postal_code_geom")
                    ))
                ),
            ),
            # Third tab content
            tabItem(tabName = "EDA",
                    box(plotOutput("plot1", height = 250)),
                    
                    box(
                        title = "Controls",
                        sliderInput("slider", "Number of observations:", 1, 100, 50)
                    )
            )
        )
    )
)