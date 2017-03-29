#
################################### Test_New ###################################
#
library(shinydashboard)
library(shiny)

## Sidebar content

sidebar <- dashboardSidebar(
              sidebarMenu(
                menuItem("Cost", tabName = "costCalc", icon = icon("building")),
                menuItem("Widgets", tabName = "widgets", icon = icon("area-chart"))
              )
            )

## Body content

body    <- dashboardBody(
              tabItems(
                # First tab content
                tabItem(tabName = "costCalc",
                  
                  fluidRow(

                        box(title = "User Inputs", status = "primary", 
                            solidHeader = TRUE, collapsible = TRUE,
                            uiOutput("choose_bbl"),
                            uiOutput("choose_material"),
                            uiOutput("choose_quality"),
                            uiOutput("choose_basement"),
                            uiOutput("choose_elevation"),
                            uiOutput("choose_water")),
                        
                        tabBox(
                          side = "left", height = "567px",
                          selected = "Tab1",
                          tabPanel("Tab1", 
                                   h4("Total Construction Cost"), 
                                   verbatimTextOutput("show_totalcost"),
                                   h4("Total Damage"), 
                                   verbatimTextOutput("show_damage")),
                          tabPanel("Tab2", "Tab content 2"),
                          tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
                        )
                        # box(
                        #    h4("Information of Selected Tax Lot"),
                        #    verbatimTextOutput("show_taxlot"),
                        #    
                        #    h4("Observations"),
                        #    tableOutput("values"),
                        #    
                        #    h4("Total Construction Cost"),
                        #    verbatimTextOutput("show_totalcost"),
                        #    
                        #    h4("Total Damage"),
                        #    verbatimTextOutput("show_damage"))
                    
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

##ORIGINAL DESIGN
# shinyUI(pageWithSidebar(
#   
#   headerPanel(""),
#   
#   sidebarPanel(
#     uiOutput("choose_bbl"),
#     
#     uiOutput("choose_material"),
#     
#     uiOutput("choose_quality"),
#     
#     uiOutput("choose_basement"),
#     
#     uiOutput("choose_elevation"),
#     
#     uiOutput("choose_water"),
#     
#     br()
#     # submitButton("Update View")
#   ),
#   
#   
#   mainPanel(
#     h4("Information of Selected Tax Lot"),
#     verbatimTextOutput("show_taxlot"),
#     
#     h4("Observations"),
#     tableOutput("values"),
#     
#     h4("Total Construction Cost"),
#     verbatimTextOutput("show_totalcost"),
#     
#     h4("Total Damage"),
#     verbatimTextOutput("show_damage")
# 
#   )
# ))
