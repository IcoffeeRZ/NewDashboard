library(shinydashboard)
library(shiny)

## Sidebar content

sidebar <- dashboardSidebar(
              sidebarMenu(
                menuItem("Damage Calculator", tabName = "costCalc", icon = icon("building")),
                menuItem("Widgets", tabName = "widgets", icon = icon("area-chart"))
              )
            )

## Body content

body    <- dashboardBody(
              tabItems(
                # First tab content
                tabItem(tabName = "costCalc",
                  
                  fluidRow(
                        box(title = "User Inputs", status = "primary", width = 4,
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
                          side = "left", height = "543px", width = 5,
                          selected = "Total Cost & Damage",
                          tabPanel("Total Cost & Damage", 
                                   h4("Total Construction Cost"), 
                                   verbatimTextOutput("show_totalcost"),
                                   br(),
                                   h4("Total Damage"), 
                                   verbatimTextOutput("show_damage"),
                                   br(),
                                   h4("Unit Costs"),
                                   tableOutput("unitCosts")),
                          tabPanel("Underlying", 
                                   h4("Building Information"),tableOutput("values"),
                                   br(),tableOutput("underlying")),
                          tabPanel("Percentage Curve",
                                   plotOutput("show_depthfunction"))
                        )
                        # box(
                        #    h4("Information of Selected Tax Lot"),
                        #    verbatimTextOutput("show_taxlot"),)
                  )      
                ),
                
                
                # Second tab content
                tabItem(tabName = "widgets",
                        h2("Widgets tab content")
                )
              )
)

# Put everything together into a dashboardPage
ui <- dashboardPage(skin = "blue",
        dashboardHeader(title = "PLUTO Dashboard"),
        
        sidebar,
        
        body
)
