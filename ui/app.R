#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#----- Importing Packages ---------
packages = c('rgdal', 'spdep', 'tmap', 'sf', 'ggpubr', 'cluster', 'factoextra', 'NbClust', 'heatmaply', 'corrplot', 'psych', 'tidyverse', 'shiny', 'shinythemes', 'shinyWidgets', 'DT', 'leaflet', 'datastructures')
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

# ---------- Data Preparation --------------------------
mpsz <- st_read(dsn = "data/geospatial", layer="MP14_SUBZONE_WEB_PL") %>%
  filter(!(PLN_AREA_N %in% c("NORTH-EASTERN ISLANDS",
                             "CENTRAL WATER CATCHMENT",
                             "CHANGI BAY",
                             "MARINA SOUTH",
                             "SIMPANG",
                             "SOUTHERN ISLANDS",
                             "STRAITS VIEW",
                             "TENGAH")))

# There are no industries under category U and T, hence it is filtered out
ssic2020_filtered <- ssic2020 %>%
    filter(!(category %in% c("U", "T")))
#--------------------- Transforming CRS for spatial data-------------------
mpsz_3414 <- st_transform(mpsz, 3414)

#------------- Converting to Spatial or Spatial Equivalents---------------------
corp_info_merged_sp <- as(corp_info_merged_sf, "Spatial")
corp_info_merged_sp <- as(corp_info_merged_sp, "SpatialPoints")
mpsz_3414_sp <- as(mpsz_3414, "Spatial")

for (category_id in unique(corp_info_merged$category)) {
    corp_with_category <- corp_info_merged_sf %>%
        filter(category == category_id)
    mpsz_3414[, paste0("Category", category_id)]<- lengths(st_intersects(mpsz_3414, corp_with_category))
}

mpsz_3414 <- mpsz_3414 %>%
    mutate(Total = rowSums(across("CategoryG":"CategoryO")))

mpsz_3414 <- mpsz_3414 %>%
    mutate(`CatGProp` = case_when(Total != 0 ~ (CategoryG/Total) / ((sum(CategoryG)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatFProp` = case_when(Total != 0 ~ (CategoryF/Total) / ((sum(CategoryF)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatHProp` = case_when(Total != 0 ~ (CategoryH/Total) / ((sum(CategoryH)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatCProp` = case_when(Total != 0 ~ (CategoryC/Total) / ((sum(CategoryC)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatNProp` = case_when(Total != 0 ~ (CategoryN/Total) / ((sum(CategoryN)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatIProp` = case_when(Total != 0 ~ (CategoryI/Total) / ((sum(CategoryI)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatSProp` = case_when(Total != 0 ~ (CategoryS/Total) / ((sum(CategoryS)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatMProp` = case_when(Total != 0 ~ (CategoryM/Total) / ((sum(CategoryM)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatQProp` = case_when(Total != 0 ~ (CategoryQ/Total) / ((sum(CategoryQ)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatLProp` = case_when(Total != 0 ~ (CategoryL/Total) / ((sum(CategoryL)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatJProp` = case_when(Total != 0 ~ (CategoryJ/Total) / ((sum(CategoryJ)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatRProp` = case_when(Total != 0 ~ (CategoryR/Total) / ((sum(CategoryR)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatPProp` = case_when(Total != 0 ~ (CategoryP/Total) / ((sum(CategoryP)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatEProp` = case_when(Total != 0 ~ (CategoryE/Total) / ((sum(CategoryE)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatKProp` = case_when(Total != 0 ~ (CategoryK/Total) / ((sum(CategoryK)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatDProp` = case_when(Total != 0 ~ (CategoryD/Total) / ((sum(CategoryD)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatAProp` = case_when(Total != 0 ~ (CategoryA/Total) / ((sum(CategoryA)/sum(Total))),
                                    Total == 0 ~ 0)) %>%
    mutate(`CatOProp` = case_when(Total != 0 ~ (CategoryO/Total) / ((sum(CategoryO)/sum(Total))),
                                    Total == 0 ~ 0))

# -------For Correlation Analysis --------------------------
mpsz_3414_derived <- st_drop_geometry(mpsz_3414)

# -------For Clustering ------------------------------------
cluster_vars <- mpsz_3414_derived %>%
  select("SUBZONE_N", ends_with("Prop"))

sg_business <- select(cluster_vars, c(2:19))
row.names(sg_business) <- cluster_vars$`SUBZONE_N`

sg_business_mat <- data.matrix(sg_business)

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
                            sidebarPanel(width=3,
                              conditionalPanel(
                                'input.edaTab === "Box Map"',
                                selectInput(inputId="EdaIndustryBoxMap",
                                            label="Select Industry",
                                            choices=c(unique(ssic2020_filtered$primary_ssic_code)),
                                            selected="AGRICULTURE AND FISHING",
                                            multiple=FALSE,
                                            width="100%"
                                ),
                                radioButtons(inputId = "valueTypeBoxMap",
                                             label = "Select Value Type",
                                             inline=TRUE,
                                             choices = list("Absolute Value", "Location Quotient"),
                                             selected = "Location Quotient"),
                              ),
                              conditionalPanel(
                                'input.edaTab === "Histogram"',
                                selectInput(inputId="EdaIndustryHist",
                                            label="Select Industry",
                                            choices=c(unique(ssic2020_filtered$primary_ssic_code)),
                                            selected="AGRICULTURE AND FISHING",
                                            multiple=FALSE,
                                            width="100%"
                                ),
                                fluidRow(
                                  column(6,
                                         checkboxInput(inputId="absoluteHist",
                                                       label="Absolute Value",
                                                       value=TRUE,
                                                       width="100%")
                                  ),
                                  column(6,
                                         checkboxInput(inputId="LQHist",
                                                       label="Location Quotient",
                                                       value=TRUE,
                                                       width="100%")
                                  )
                                ),
                              ),
                              conditionalPanel(
                                'input.edaTab === "Correlation Analysis"',
                                selectInput(inputId="ClusterEDAfields",
                                            label="Measure",
                                            choices=c(unique(ssic2020_filtered$primary_ssic_code)),
                                            selected=c(unique(ssic2020_filtered$primary_ssic_code)),
                                            multiple=TRUE,
                                            width="100%"
                                ),
                              
                              ),
                            ),
                            
                            mainPanel(width=9,
                              fluidRow(
                                column(10, align="center", offset = 1,
                                       tabsetPanel(
                                         id = "edaTab",
                                         tabPanel("Box Map", br(),
                                                  leafletOutput(outputId="boxMapOutput", width="800px", height="800px")
                                                  ),
                                         tabPanel("Histogram", br(),
                                                  conditionalPanel(
                                                    'input.absoluteHist',
                                                      column(6,
                                                           plotlyOutput(outputId="edaOutput1", width = "100%", height = "400px", inline = FALSE)
                                                      )
                                                    ),
                                                  conditionalPanel(
                                                    'input.LQHist',
                                                    column(6,
                                                           plotlyOutput(outputId="edaOutput2", width = "100%", height = "400px", inline = FALSE)
                                                    )
                                                  )
                                        ),
                                         tabPanel("Correlation Analysis",
                                                  plotOutput("edaCorrPlot",
                                                             width="800px",
                                                             height="800px"))
                                       )
                                )
                              )
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
                                      
                                      # Kernel Density Map
                                      mainPanel(
                                      )
                        )
               ),
               # ----- Clustering Panel -------------------
               tabPanel("Clustering", value="clustering", fluid=TRUE, icon=icon("globe-asia"),
                        sidebarLayout(fluid=TRUE,
                                      sidebarPanel(fluid=TRUE, width=3,
                                                   selectInput(inputId="proximityCal",
                                                               label="Proximity Calculation",
                                                               choices=c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                                                               selected="euclidean",
                                                               multiple=FALSE,
                                                               width="100%"
                                                   ),
                                                   selectInput(inputId="agglomerationMethod",
                                                               label="Agglomeration Method",
                                                               choices=c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                                                               selected="ward.D",
                                                               multiple=FALSE,
                                                               width="100%"
                                                   ),
                                                   sliderInput("Clusters",
                                                               "Number of Clusters:",
                                                               min = 2,
                                                               max = 10,
                                                               value = 5),
                                                   radioButtons(inputId = "clusteringMethod",
                                                                label = "Select Clustering Method",
                                                                inline=TRUE,
                                                                choices = list("Hierarchical", "Skater"),
                                                                selected = "Hierarchical"),
                                      ),
                                      
                                      # Show Clustering Plots
                                      mainPanel(fluid=TRUE, width=9,
                                                fluidRow(
                                                  column(6, leafletOutput("clusteringPlot")),
                                                  column(6, plotOutput("clusterDendrogram"))
                                                ),
                                                conditionalPanel("input.agglomerationMethod != 'median' & input.agglomerationMethod != 'centroid'",
                                                fluidRow(
                                                    column(6, plotlyOutput("geographicSeg"))
                                                  )
                                                )
                               )
                        )
               )
    )
)

# Define server logic 
server <- function(input, output) {
    
    #-------------- Data Page --------------------
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
    
    #-------------- EDA ----------------------------
    
    ## ------ EDA Box Map ------------------
    boxbreaks <- function(v,mult=1.5) {
      qv <- unname(quantile(v,na.rm = TRUE))
      iqr <- qv[4] - qv[2]
      upfence <- qv[4] + mult * iqr
      lofence <- qv[2] - mult * iqr
      # initialize break points vector
      bb <- vector(mode="numeric"
                   ,length=7)
      # logic for lower and upper fences
      if (lofence < qv[1]) { # no lower outliers
        bb[1] <- lofence
        bb[2] <- floor(qv[1])
      } else {
        bb[2] <- lofence
        bb[1] <- qv[1]
      }
      if (upfence > qv[5]) { # no upper outliers
        bb[7] <- upfence
        bb[6] <- ceiling(qv[5])
      } else {
        bb[6] <- upfence
        bb[7] <- qv[5]
      }
      bb[3:5] <- qv[2:4]
      return(bb)
    }
      
      get.var <- function(vname,df) {
        v <- df[vname] %>% st_set_geometry(NULL)
        v <- unname(v[,1])
        return(v)
      }
      
      boxmap <- function(vnam, df, legtitle=NA, mtitle="Box Map", mult=1.5){
        var <- get.var(vnam,df)
        bb <- boxbreaks(var)
        
        tm_shape(df) +
          tm_fill(vnam,title=legtitle,breaks=bb,palette="-RdBu", id="SUBZONE_N",
                  labels = c("lower outlier",
                             "< 25%",
                             "25% - 50%",
                             "50% - 75%",
                             "> 75%",
                             "upper outlier")) +
        tm_borders(alpha=0.5) +
          tm_layout(title = mtitle, title.position = c("right", "bottom"))
      }
      
      output$boxMapOutput <- renderLeaflet({
        # initialize hash
        industry = new.env(hash = TRUE, parent = emptyenv(), size = 100L)
        
        if(input$valueTypeBoxMap == 'Location Quotient'){
          # assign values to keys
          keys <- c(unique(ssic2020_filtered$primary_ssic_code))
          value <- paste("Cat", unique(ssic2020_filtered$category), "Prop", sep="")
          
          assign_hash(keys, value, industry)
          
          tmap_leaflet(boxmap(get_hash(input$EdaIndustryBoxMap, industry), mpsz_3414) , in.shiny=TRUE)
        } else {
          # assign values to keys
          keys <- c(unique(ssic2020_filtered$primary_ssic_code))
          value <- paste("Category", unique(ssic2020_filtered$category), sep="")
          
          assign_hash(keys, value, industry)
          
          tmap_leaflet(boxmap(get_hash(input$EdaIndustryBoxMap, industry), mpsz_3414) , in.shiny=TRUE)
        }
      })
      
    ## ------ EDA Histogram ----------------
    # vectorize assign, get and exists for convenience
    assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))
    get_hash <- Vectorize(get, vectorize.args = "x")
    exists_hash <- Vectorize(exists, vectorize.args = "x")
    
    
    observe({
      # If Absolute Value for Histogram is selected on checkbox
      if (input$absoluteHist){
        
        # initialize hash
        abCategory = new.env(hash = TRUE, parent = emptyenv(), size = 100L)
        # assign values to keys
        keys <- c(unique(ssic2020_filtered$primary_ssic_code))
        value <- paste("Category", unique(ssic2020_filtered$category), sep="")
        
        assign_hash(keys, value, abCategory)
        
        output$edaOutput1 <- renderPlotly({
          ggplotly(ggplot(data=mpsz_3414, aes_string(x= get_hash(input$EdaIndustryHist, abCategory))) + 
                     geom_histogram(bins = 20) + xlab(input$EdaIndustryHist) + ggtitle("Absolute Value"))
          
        })
      }
      
      # If Location Quotient for Histogram is selected on checkbox
      if (input$LQHist){
        
        # initialize hash
        LQCategory = new.env(hash = TRUE, parent = emptyenv(), size = 100L)
        # assign values to keys
        keys <- c(unique(ssic2020_filtered$primary_ssic_code))
        value <- paste("Cat", unique(ssic2020_filtered$category), "Prop", sep="")
        
        assign_hash(keys, value, LQCategory)
        
        output$edaOutput2 <- renderPlotly({
          ggplotly(ggplot(data=mpsz_3414, aes_string(x= get_hash(input$EdaIndustryHist, LQCategory))) + 
                     geom_histogram(bins = 20) + xlab(input$EdaIndustryHist)  + ggtitle("Location Quotient"))
          
        })
      }
    })      
    
    ## ------ EDA Clustering ----------------
    output$edaCorrPlot <- renderPlot({
      # initialize hash
      LQCategory = new.env(hash = TRUE, parent = emptyenv(), size = 100L)
      # assign values to keys
      keys <- c(unique(ssic2020_filtered$primary_ssic_code))
      value <- paste("Cat", unique(ssic2020_filtered$category), "Prop", sep="")
      
      assign_hash(keys, value, LQCategory)
      
      
      varSelected <- input$ClusterEDAfields
      
      cluster_vars.cor = cor(mpsz_3414_derived[, get_hash(c(varSelected), LQCategory)])
      
      corrplot.mixed(cluster_vars.cor,
                     lower = "ellipse", 
                     upper = "number",
                     tl.pos = "lt",
                     diag = "l",
                     tl.col = "black")
    })
    
    
    #-----------Clustering -------------------------------------------
    output$clusterDendrogram <- renderPlot({
      proxmat <- dist(sg_business, method= input$proximityCal)
      
      hclust_ward <- hclust(proxmat, method= input$agglomerationMethod)
      
      plot(hclust_ward, cex = 0.6)
      rect.hclust(hclust_ward, k = input$Clusters, border = 2:5)
    })
    
    output$geographicSeg <- renderPlotly({
      heatmaply(normalize(sg_business_mat),
                Colv=NA,
                dist_method = input$proximityCal,
                hclust_method = input$agglomerationMethod,
                seriate = "OLO",
                colors = Blues,
                k_row = input$Clusters,
                margins = c(NA,200,60,NA),
                fontsize_row = 4,
                fontsize_col = 5,
                main="Geographic Segmentation of Singapore by Business Prominence",
                xlab = "Business Prominence",
                ylab = "Singapore's Planning Areas"
      )
    })
    
    output$clusteringPlot <- renderLeaflet({
      proxmat <- dist(sg_business, method= input$proximityCal)
      hclust_ward <- hclust(proxmat, method= input$agglomerationMethod)
      
      groups <- as.factor(cutree(hclust_ward, k=input$Clusters))
      
      sg_biz_cluster <- cbind(mpsz_3414, as.matrix(groups)) %>%
        rename(`CLUSTER`=`as.matrix.groups.`)
      
      if(input$clusteringMethod == "Hierarchical"){
        cluster_map <- tm_shape(sg_biz_cluster)+
          tm_fill(col="CLUSTER", id="SUBZONE_N",
                  title=" Indicator Clusters")+
          tm_borders(alpha=0.5)
      }
      
      if (input$clusteringMethod == "Skater") {
        # -----SKATER -----
        mpsz_3414_sp <- mpsz_3414_sp[!mpsz_3414$SUBZONE_N %in% c("SUDONG", "SEMAKAU"),]
        sg.nb <- poly2nb(mpsz_3414_sp)
        lcosts <- nbcosts(sg.nb, sg_business)
        sg.w <- nb2listw(sg.nb, lcosts, style="B")
        sg.mst <- mstree(sg.w)
        
        skaterclust <- skater(sg.mst[,1:2], sg_business, method = input$proximityCal, input$Clusters - 1)
        
        clustergrps <- skaterclust$groups
        groups_mat <- as.matrix(skaterclust$groups)
        sg_biz_cluster <- sg_biz_cluster %>%
          filter(!SUBZONE_N %in% c("SUDONG", "SEMAKAU"))
        sg_biz_spatialcluster <- cbind(sg_biz_cluster, as.factor(groups_mat)) %>%
          rename(`SP_CLUSTER`=`as.factor.groups_mat.`)
        
        cluster_map <- tm_shape(sg_biz_spatialcluster)+
          tm_fill(col="SP_CLUSTER", id="SUBZONE_N",
                  title=" Indicator Clusters")+
          tm_borders(alpha=0.5)
        
      }
      tmap_leaflet(cluster_map, in.shiny=TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
