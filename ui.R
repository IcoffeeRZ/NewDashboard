#
################################### Test_New ###################################
#

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel(""),
  
  sidebarPanel(
    uiOutput("choose_bbl"),
    
    uiOutput("choose_material"),
    
    uiOutput("choose_quality"),
    
    uiOutput("choose_basement"),
    
    uiOutput("choose_elevation"),
    
    uiOutput("choose_water"),
    
    br()
    # submitButton("Update View")
  ),
  
  
  mainPanel(
    h4("Information of Selected Tax Lot"),
    verbatimTextOutput("show_taxlot"),
    
    h4("Observations"),
    tableOutput("values"),
    
    h4("Total Construction Cost"),
    verbatimTextOutput("show_totalcost"),
    
    h4("Total Damage"),
    verbatimTextOutput("show_damage")

  )
))
