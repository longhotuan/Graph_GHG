#### Global R ####

library(knitr)
library(rmarkdown)
library(tidyverse)
library(reshape)
library(plotly)
library(flexdashboard)
library(shiny)
library(data.table)
library(lubridate)
library(shinythemes)
library(shinydashboard)
library(tweenr)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(rsconnect)
library(DT)
library(rworldmap)
library(feather)

ghg_cuenca <- read_csv("River_WQI2.csv")
first_column <- which(colnames(ghg_cuenca) == "Water temperature (oC)")
last_column <- which(colnames(ghg_cuenca) == "N2O Flux (mg/m2/d)")

#### ui ####

ui <- dashboardPage(skin = "green",
                    # Dashboard header ####
                    dashboardHeader(title="Greenhouse gas emissions from Cuenca rivers"),
                    # Dashboard sidebar #### 
                    dashboardSidebar(
                        sidebarMenu(id="tabs",
                                    menuItem("About", 
                                             tabName = "about",
                                             icon = icon("info")),
                                    menuItem("Site description", 
                                             tabName = "info",
                                             icon = icon("microscope")),
                                    conditionalPanel(condition = "input.tabs == 'info'",
                                                     selectInput(inputId = "water", label = "Select a variable", 
                                                                 choices = c(All = "All", "Partner countries", colnames(ghg_cuenca)[first_column:last_column])))
                                    )
                        
                    ),
                    # Dashboard body #### 
                    dashboardBody(
                        tabItems(
                            # About tab content ####
                            tabItem(tabName = "about",
                                    fluidRow(
                                        box(width = 12, 
                                            h2("Abstract"),
                                            
                                            br(),
                                            h4("Besides serving as conduits for greenhouse gas (GHG) emissions from groundwater and sediments to the atmosphere, rivers also act as a natural source of GHGs released via the metabolisms of aquatic organisms. Anthropogenic activities can largely alter the chemical composition of rivers, thus affecting microbial composition and consequently the GHG emissions. To investigate these impacts, we assessed the flux of carbon dioxide (CO2), nitrous oxide (N2O), and methane (CH4) from the five major tributaries of the Cuenca urban river system (Ecuador). High variabilities of the emissions were found among the tributaries, which appears to be dependent mainly on water quality and neighboring landscapes. Particularly, being the most polluted tributary, Tomebamba tributary released more than double the mean emission, i.e. 186.8±51.8 Gg CO2 yr-1, 1.5±0.6 Gg CH4 yr-1, and 73.1±22.2 Mg N2O yr-1, accounting for 57.0% of the total CO2 emission, 76.7% of the total CH4 emission, and 44.6% of the total N2O emission from the whole basin. High peaks of GHG fluxes were found in the Cuenca tributary after the discharge of the Ucubamba wastewater treatment plant (WWTP). Conversely, having major sites with the good and acceptable water quality, Machangara and Yanuncay emitted only 5% of the total emission of CO2 and N2O, and 0.7% of the total CH4 emission. By applying Prati and Oregon indexes, a clear pattern between water quality and GHG fluxes was observed, in which the more polluted the sampling sites were, the higher their GHG fluxes were produced. In addition, land-use types surrounding the rivers, such as urban, transportation systems, and agriculture, had strong impacts on water quality and GHG emissions. These findings suggest the negative impact of anthropogenic activities on river GHG emissions via WWTP discharges and surface runoff. Lastly, random forests identified dissolved oxygen, ammonium, and flow characteristics as the main influential factors on the GHG emissions. Conversely, low impact of organic matter and nitrate concentration implies a higher role of nitrification than denitrification in producing N2O in the rivers."),
                                          
                                        )
                                    ),
                                    fluidRow(
                                        box(width = 12, 
                                            h2("Acknowledgment"),
                                            
                                            br(),
                                            h4("This research was performed in the context of the VLIR Ecuador Biodiversity Network project. This project was funded by the Vlaamse Interuniversitaire Raad-Universitaire Ontwikkelingssamenwerking (VLIR-UOS), which supports partnerships between universities and university colleges in Flanders and the South. We thank Carlos Santiago Deluquez, Caio Neves, Paula Avila, Juan Enrique Orellana, and Kate Pesantez for their contributions during the sampling campaign. We are grateful to the Water and Soil Quality Analysis Laboratory of the University of Cuenca for their supports in our analyses."),
                                            
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               h1("Funded by"),
                                               img(style = "max-width:30%",
                                                   src = "Logo2.jpg")
                                        ),
                                        column(6, 
                                               img(align = "left|bottom",
                                                   style = "max-width:20%",
                                                   src = "Logo.png") 
                                        )
                                    )
                            ), # end of About tabItem
                            # Info tab content ####
                            tabItem(tabName = "info",
                                    fluidRow(
                                        box(title = "Value of the variable at each sampling site", width = 12, height = 500, 
                                            leafletOutput("map", width = "100%", height = 500) # Can be changed
                                        )),
                                    fluidRow(
                                        box(title = "Summary", width = 12, heigh = 200,
                                            verbatimTextOutput("table"))
                                        
                                    )
                            ) # end of Info tab 
                        ) # end tabItems
                    ) # end dashboardbody
) # end dashboardpage

#### server ####

server <- function(input, output,session) {
    # Setting reactivities ####
    df <- reactive({ghg_cuenca})
    variablename <- reactive({
        colnames(df())[first_column:last_column]
    })
    observe({
        updateSelectInput(session, inputId = "water", label = "Select a variable", choices = c(variablename()))
    })
    df_variable <- reactive({
        input$water
    })

    # Output map in Info tab ####
    output$map <- renderLeaflet({
        selecteddf2 <- df()
        colors <- brewer.pal(n = 7, name = "Dark2")
        tilesURL <- '//{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png'
        leaflet() %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -79.10, lat1 =  -2.83, lng2 = -78.85, lat2 = -2.963) %>%
            addMinicharts(selecteddf2$Longitude, selecteddf2$Latitude,
                          type = "pie",
                          chartdata = selecteddf2[,which(colnames(selecteddf2) == df_variable())],
                          colorPalette = colors,
                          opacity = 0.6,
                          showLabels = TRUE,
                          transitionTime = 0)
    })
    
    # Output table in Info tab ####
    
    output$table <- renderPrint(summary(df()[[input$water]]))
}

#### Run the application ####
shinyApp(ui = ui, server = server)