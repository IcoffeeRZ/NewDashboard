#
################################### Test_New ###################################
#
library(shinydashboard)
library(shiny)

server <- function(input, output) {
  
  # # BBL
  output$choose_bbl <- renderUI({
    selectizeInput("bbl", label = "BBL of a Tax Lot", choices = allBBL)  # input$bbl
  })
  
  bblInput <- reactive({
    currentBBL <- input$bbl
  })
  
  # bblRow <- reactive({
  #   currentRow <- mn[mn$BBL == input$bbl, ]      
  # })
  
  # # Material
  output$choose_material <- renderUI({
    if(is.null(input$bbl))  
      return()
    lot <- mn[mn$BBL == input$bbl, ]
    # materialOptions: a vector contains all possible material options
    
    if(all(is.na(lot[matLogic])))
      return("No available material!")
    materialOptions <- materialList[which(!is.na(lot[matLogic]))]

    #!!!!!!!!!!!!!!!!!!!!!!!!
    selectizeInput("material", "Choose a material:", 
                   choices = materialOptions, selected = materialOptions[1])
    # default option is the 1st element in materialOptions
    
  })
  
  output$choose_quality <- renderUI({
    if(is.null(input$bbl))       
      return()
    if (is.null(input$material)) 
      return()
    if (is.na(input$material))   
      return()
    # lot <- mn[mn$BBL == input$bbl, ]
    # btype <- typeidList[which(materialList == input$material)]
    # id <- as.numeric(lot[btype])  # buildingtypeid
    # if (is.null(id)) # if missing material, return to avoid error
    #   return()
    # if (is.na(id))   # if missing material, return to avoid error
    #   return()
    # qclasses <- as.vector(t(desc[which(desc$buildingtypeid == id), 2:7]))
    qualityOptions <- get_QualityClasses(get_Typeid(input$bbl, input$material, mn))
    selectizeInput("quality", "Choose a construction quality:", 
                   choices = qualityOptions, selected = qualityOptions[1])
    # default option is the 1st element in materialOptions
  })
  
  output$choose_basement <- renderUI({
    radioButtons("basement", label = "Basement",
                 choices = list("Yes" = TRUE, "No" = FALSE), 
                 selected = TRUE)
  })
  
  output$choose_elevation <- renderUI({
    sliderInput("elevation", "First Floor Elevation:",
                min=0, max=25, value=0)
  })  

  output$choose_water <- renderUI({
    sliderInput("water", "Water Depth:",
                min=-4, max=24, value=0)
  })      
  
  inputValues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("BBL", 
               "Material",
               "Quality",
               "Basement",
               "In-structure Water Depth"),
      
      Value = as.character(c(input$bbl, 
                             input$material,
                             input$quality,
                             ifelse(input$basement, "With Basement", "Without Basement"),
                             paste(input$water - input$elevation, "ft")
                            )), 
      stringsAsFactors=FALSE)
  }) 
  
  computeCost <- reactive({
    quality <- which(get_QualityClasses(get_Typeid(input$bbl, input$material, mn)) == input$quality)
    return(get_TotalCost(BBL = input$bbl, Material = input$material, 
                  QualityClassNum  = quality, 
                  BsmtType = input$basement,
                  PlutoTable = mn))
    
  }) 

  computeDamage <- reactive({
    pr <- mn[mn$BBL == input$bbl, ]
    bclass <- get_BldgClass(BBL = input$bbl, PlutoTable = mn)

    curve <- get_DamageProbs(BldgClass = bclass, BsmtType = input$basement,
                    FloodType = pr$flood_type)
    depth <- input$water - input$elevation
    # depth <- ifelse(depth < -4, -4, depth)
    depth <- ifelse(depth > 24, 24, depth)
    p.damage <- ifelse(depth < -4, 0, curve[depth+5])
    # p.damage <- curve[depth+5]
    damage <- p.damage * computeCost()
    damage #!!!!!!!!!!!! return(damage)
  })
  
  #++++++++++++++++++++++++++++++++++ OUTPUT +++++++++++++++++++++++++++++++++++
  # # Show row information of BBL
  output$show_taxlot <- renderPrint({
    data.frame(mn[mn$BBL == input$bbl, displayColList])
  })
  
  # # Show the input values using an HTML table
  output$values <- renderTable({
    if(is.null(input$bbl))       
      return()
    if (is.null(input$material)) 
      return()
    # if (anyNA(input$material)) 
    #   return()
    if (is.null(input$quality))  
      return()
    # if (anyNA(input$quality))
    #   return()
    if (is.null(input$basement))
      return()

    inputValues()
  })
  
  output$show_totalcost <- renderPrint({
    if(is.null(input$bbl))      
      return()
    if (is.null(input$material)) 
      return()
    # if (is.na(input$material)) 
    #   return()
    # if (anyNA(input$material)) 
    #   return()
    if (is.null(input$quality)) 
      return()
    # if (anyNA(input$quality))
    #   return()
    # if (is.null(input$basement))
    #   return()
    
    computeCost()
  })
  
  output$show_damage <- renderPrint({
    if(is.null(input$bbl))       
      return()
    if (is.null(input$material)) 
      return()
    # if (anyNA(input$material))
      # return()
    if (is.null(input$quality))   
      return()
    # if (anyNA(input$quality))
      # return()
    if (is.null(input$basement)) 
      return()
    # if (is.null(computeCost()))
      # return()
    # if (anyNA(computeCost()))
      # return()
    computeDamage() #!!!!!!!!!!!!!!
  })
  
}
