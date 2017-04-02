library(shinydashboard)
library(shiny)

## Sidebar content

sidebar <- dashboardSidebar(
              sidebarMenu(
                menuItem("Damage Calculator", tabName = "costCalc", icon = icon("calculator")),
                menuItem("Satellite View", tabName = "satelliteView", icon = icon("street-view")),
                menuItem("Interactive Map", tabName = "interMap", icon = icon("map-o")),
                menuItem("Property Explorer", tabName = "explorer", icon = icon("building"))
              )
            )

## Body content

body    <- dashboardBody(
              tabItems(
                # First tab content
                tabItem(tabName = "costCalc",
                  
                  fluidRow(
                        box(title = "User Inputs", status = "primary", width = 3,
                            solidHeader = TRUE, collapsible = TRUE,
                            uiOutput("choose_bbl"),
                            uiOutput("choose_material"),
                            uiOutput("choose_quality"),
                            uiOutput("choose_basement"),
                            uiOutput("choose_elevation"),
                            uiOutput("choose_water"),
                            actionButton(inputId = "go", label = "Calculate Damage", 
                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                         icon = icon("desktop"))),
                        
                        tabBox(
                          side = "left", height = "700px", width = 4,
                          selected = "Total Cost & Damage",
                          tabPanel("Total Cost & Damage", 
                                   h4("Building Information"), 
                                   tableOutput("underlying"),
                                   # verbatimTextOutput("show_totalcost"),
                                   br(),
                                   h4("Total Damage"), 
                                   verbatimTextOutput("show_damage"),
                                   br(),
                                   h4("Unit Costs"),
                                   tableOutput("unitCosts")),
                          tabPanel("Underlying", 
                                   h4("Building Information"),tableOutput("values"),
                                   br(),
                                   h4("Percentage Curve"), plotOutput("show_depthfunction"))
                        ),
                        
                        box(title = "Street View", status = "primary", width = 5,
                            collapsible = TRUE, 
                            height = "700px",
                            htmlOutput("street_pic"))
                        
                  )
                  
                ),
                
                
                # Second tab content
                tabItem(tabName = "satelliteView",
                        google_mapOutput("satellite_view", width = "100%", height = "800px")
                ),
                # Third tab content
                tabItem(tabName = "interMap",
                        h2("Under Construction")
                ),
                # Fourth tab content
                tabItem(tabName = "explorer",
                        fluidRow(
                          box(title = "Columns in PLUTO", status = "primary",
                              width = 10, solidHeader = TRUE, collapsible = TRUE,
                            checkboxGroupInput('show_vars', 'Columns in PLUTO:',
                                               colnames(mn), selected = colnames(mn),inline = T)
                          ),
                  
                          DT::dataTableOutput('mytable1')

                        )
                        
                )
              )
)

# Put everything together into a dashboardPage
ui <- dashboardPage(skin = "blue",
        dashboardHeader(title = "PLUTO Dashboard"),
        
        sidebar,
        
        body
)
