#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#----- Importing Packages ---------
packages = c('rgdal', 'spdep', 'tmap', 'sf', 'ggpubr', 'cluster', 'factoextra', 'NbClust', 'heatmaply', 'corrplot', 'psych', 'tidyverse', 'shiny', 'shinythemes', 'shinyWidgets', 'DT')
for (p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}

#-----Importing Aspatial and Spatial Data ------------
corp_info_merged <- read_csv("data/aspatial/corp_info_merged.csv")
corp_info_merged_sf <- st_as_sf(corp_info_merged, coords = c('X_coord','Y_coord'), crs = 3414)

ssic2020 <- read_csv("data/aspatial/ssic2020.csv")

postal_code_geom <- read_csv("data/aspatial/postal_code_geom.csv")

mpsz <- st_read(dsn = "data/geospatial", layer="MP14_SUBZONE_WEB_PL")

# ---------- Data Preparation --------------------------
planning_area <- mpsz %>%
    group_by(PLN_AREA_N) %>%
    summarise(geometry = st_union(geometry)) %>%
    ungroup() %>%
    filter(!(PLN_AREA_N %in% c("NORTH-EASTERN ISLANDS",
                               "CENTRAL WATER CATCHMENT",
                               "CHANGI BAY",
                               "MARINA SOUTH",
                               "SIMPANG",
                               "SOUTHERN ISLANDS",
                               "STRAITS VIEW",
                               "TENGAH")))

#--------------------- Transforming CRS for spatial data-------------------
planning_area_3414 <- st_transform(planning_area, 3414)

#------------- Converting to Spatial or Spatial Equivalents---------------------
corp_info_merged_sp <- as(corp_info_merged_sf, "Spatial")
corp_info_merged_sp <- as(corp_info_merged_sp, "SpatialPoints")
planning_area_sp <- as(planning_area_3414, "Spatial")

for (category_id in unique(corp_info_merged$category)) {
    corp_with_category <- corp_info_merged_sf %>%
        filter(category == category_id)
    planning_area_3414[, paste0("Category ", category_id)]<- lengths(st_intersects(planning_area_3414, corp_with_category))
}

planning_area_3414 <- planning_area_3414 %>%
    mutate(Total = rowSums(across("Category G":"Category O")))

planning_area_3414 <- planning_area_3414 %>%
    mutate(`Cat G Prop` = case_when(Total != 0 ~ `Category G`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat F Prop` = case_when(Total != 0 ~ `Category F`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat H Prop` = case_when(Total != 0 ~ `Category H`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat C Prop` = case_when(Total != 0 ~ `Category C`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat N Prop` = case_when(Total != 0 ~ `Category N`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat I Prop` = case_when(Total != 0 ~ `Category I`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat S Prop` = case_when(Total != 0 ~ `Category S`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat M Prop` = case_when(Total != 0 ~ `Category M`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat Q Prop` = case_when(Total != 0 ~ `Category Q`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat L Prop` = case_when(Total != 0 ~ `Category L`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat J Prop` = case_when(Total != 0 ~ `Category J`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat R Prop` = case_when(Total != 0 ~ `Category R`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat P Prop` = case_when(Total != 0 ~ `Category P`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat E Prop` = case_when(Total != 0 ~ `Category E`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat K Prop` = case_when(Total != 0 ~ `Category K`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat D Prop` = case_when(Total != 0 ~ `Category D`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat A Prop` = case_when(Total != 0 ~ `Category A`/Total * 1000,
                                    Total == 0 ~ 0)) %>%
    mutate(`Cat O Prop` = case_when(Total != 0 ~ `Category O`/Total * 1000,
                                    Total == 0 ~ 0))


# ------- UI -----------
ui <- fluidPage(theme=shinytheme("darkly"),
                
    # -----Navigation Bar ---------------------
    navbarPage("LandMarkdown", fluid=TRUE, windowTitle="LandMarkdown", selected="overview",
               
               # ----- Overview Panel -------------------
               tabPanel("Overview", value="overview", fluid=TRUE, icon=icon("dashboard"),
                        sidebarLayout(
                            sidebarPanel(
                                fluidRow(
                                    column(10, offset=1, align="center", 
                                           h2("List of Pages:")
                                    )),
                                fluidRow(
                                    div(style="background-color: darkslategray; border-radius:3px; padding: 20px; margin: 20px;", h3("Data"), p("This consist of the dataset used for our analysis. In this section, you will be able to see the raw data and what each columns
                                  signifies."))),
                                fluidRow(
                                    div(style="background-color: steelblue; border-radius:3px; padding: 20px; margin: 20px;", h3("EDA"), p("Exploratory Data Analysis. This is the stage where we try to understand the data with maps and charts"))),
                                fluidRow(
                                    div(style="background-color: coral; border-radius:3px; padding: 20px; margin: 20px;", h3("SPP"), p("Spatial Points Pattern. Visualise the areas that are more concentrated with selected industry."))),
                                fluidRow(
                                    div(style="background-color: cadetblue; border-radius:3px; padding: 20px; margin: 20px;", h3("Clustering"), p("Which are the few subzones that have similar industries in the area? How are they grouped together?")))
                            ),
                            # Description of project
                            mainPanel(
                                fluidRow(
                                    column(10, offset=1, style = "background-color: #303030; border-radius:3px; padding: 30px; margin: 20px; font-size: 18px;",
                                           h1("Introduction", align="center"), br(),
                                           p("Over the years, planning for Singapore’s long term sustainability and optimising usage of our limited land have always been a 
                              strategic challenge for Singapore’s government agencies. The Urban Redevelopment Authority (URA) has been launching initiatives such
                              as the Concept Plan and Master Plan in an effort to develop Singapore’s physical environment in a sustainable manner. However, data 
                              shared by various agencies at public online data portals have not been explored or worked on extensively within their counterparts to 
                              gain a better understanding on the spatial distribution of businesses in Singapore and thus unable to more effectively translate them 
                              into better insights."),
                                           p("Our team feels that we can contribute to this area by demonstrating how we can make use of data provided by these open data portals 
                              and analyse them together in order to showcase the role of geospatial analytics in Singapore."))
                                    
                                ),
                                br(),
                                fluidRow( 
                                    column(10, offset=1, style="background-color: #303030; border-radius:3px; padding: 30px; margin: 20px; font-size:18px",
                                           h1("Project Objectives", align="center"), br(),
                                           p("For this project, we would be creating an analytical tool for observing and analysing the spatial point patterns of business entities in Singapore. Specifically, it focuses on the following objectives:"),
                                           tags$li("To visualise the spatial distribution of different business types in Singapore on an internet-based map such as OpenStreetMap."),
                                           tags$li("To conduct statistical simulations to reveal evidence of clusters using spatial point patterns analysis."),
                                           tags$li("To conduct clustering to reveal characteristics of spatially constrained clusters."),
                                           tags$li("To provide a user-friendly interface through the creation of a RShiny application to apply relevant filters for different business types."))
                                )
                                
                            )
                        )
               ),
              
               # ----- Data Panel -------------------
               tabPanel("Data", value="data", fluid=TRUE, icon=icon("database"),
                      fluidRow(
                          column(10, align="center", offset = 1,
                                tabsetPanel(
                                    id = "datatable",
                                    tabPanel("Corporate Entities Information", br(),
                                             p("This dataset comprises data on entities that are registered in Singapore, with additional information such as 
                                                       paid-up capital."), br(), DT::dataTableOutput('corp_info_table')),
                                    
                                    tabPanel("Singapore Standard Industrial Classification", br(),
                                             p("The Singapore Standard Industrial Classification (SSIC) is the national standard for classifying economic activities 
                                               undertaken by economic units."), br(), DT::dataTableOutput('ssic2020')),
                                    tabPanel("OneMap Address Data Search", br(),
                                             p("This search API returns the geometry of an address based on search values such as postal codes."), 
                                             br(), DT::dataTableOutput('postal_code_geom'))
                                )
                            )
                        )
               ),
               # ----- EDA Panel -------------------
               tabPanel("EDA", value="eda", fluid=TRUE, icon=icon("chart-pie"),
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(fluid=TRUE,
                            sidebarPanel(
                                div(style = "max-height: 40vh; overflow-y: auto;", 
                                    title = "Industries", width = 3,
                                    uiOutput("industries")
                                )
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                            )
                        )
               ),
               # ----- SPP Panel -------------------
               tabPanel("SPP", value="spp", fluid=TRUE, icon=icon("map-marker-alt"),
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(fluid=TRUE,
                                      sidebarPanel(
                                          sliderInput("bins",
                                                      "Number of bins:",
                                                      min = 1,
                                                      max = 50,
                                                      value = 30)
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                      )
                        )
               ),
               # ----- Clustering Panel -------------------
               tabPanel("Clustering", value="clustering", fluid=TRUE, icon=icon("globe-asia"),
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(fluid=TRUE,
                                      sidebarPanel(fluid=TRUE,
                                                   sliderInput("bins",
                                                               "Number of bins:",
                                                               min = 1,
                                                               max = 50,
                                                               value = 30)
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(fluid=TRUE,
                                                plotOutput("distPlot")
                                      )
                        )
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$corp_info_table = DT::renderDataTable({
        DF <- as.data.frame(corp_info_merged)
        DF <- DF %>% 
            mutate_if(is.numeric, round, digits = 3)
        DT::datatable(DF, style = "bootstrap")
        })
    output$ssic2020 = DT::renderDataTable({
        DF <- as.data.frame(ssic2020)
        DF <- DF %>% 
            mutate_if(is.numeric, round, digits = 3)
        DT::datatable(DF, style = "bootstrap")
    })
    output$postal_code_geom = DT::renderDataTable({
        DF <- as.data.frame(postal_code_geom)
        DF <- DF %>% 
            mutate_if(is.numeric, round, digits = 3)
        DT::datatable(DF, style = "bootstrap")
    })
    
    output$industries <- renderUI({checkboxGroupInput('selected_industries',
                                                      '',
                                                      c(unique(ssic2020$primary_ssic_code)))})
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
