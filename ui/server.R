#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

# Importing Aspatial and Spatial Data
corp_info_merged <- read_csv("data/aspatial/corp_info_merged.csv")

ssic2020 <- read_csv("data/aspatial/ssic2020.csv")

postal_code_geom <- read_csv("data/aspatial/postal_code_geom.csv")

mpsz <- st_read(dsn = "data/geospatial", layer="MP14_SUBZONE_WEB_PL")


shinyServer(function(input, output, session) {
    # The overview tab will update as active when the logo is clicked
    observeEvent(input$Overview, {
        updateTabItems(session, "sidebar", "Overview")
    })
    
    #Data Table
    output$corp_info_table <- DT::renderDataTable({
        corp_info_merged 
    })
    
    output$ssic2020 <- DT::renderDataTable({
        ssic2020 
    })
    
    output$postal_code_geom <- DT::renderDataTable({
        postal_code_geom 
    })
    

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
