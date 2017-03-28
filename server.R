#
################################### Test_New ###################################
#
library(shiny)

shinyServer(function(input, output) {
  
  # # BBL
  output$choose_bbl <- renderUI({
    selectizeInput("bbl", label = "BBL of a Tax Lot", choices = allBBL)  # input$bbl
  })
  
  bblInput <- reactive({
    currentBBL <- input$bbl
  })
  
  bblRow <- reactive({
    currentBBL <- mn[which(mn$bbl == bblInput()), ]      
  })
  
  # # Material
  output$choose_material <- renderUI({
    if(is.null(input$bbl))  # if missing BBL, return to avoid error
      return()
    lot <- bblRow()
    # materialOptions: a vector contains all possible material options
    materialOptions <- materialList[as.vector(t(lot[matLogic]))]
    if(all(is.na(materialOptions)))
      return()
    #!!!!!!!!!!!!!!!!!!!!!!!!
    selectizeInput("material", "Choose a material:", 
                   choices = materialOptions, selected = materialOptions[1])
    # default option is the 1st element in materialOptions
    
  })
  
  output$choose_quality <- renderUI({
    if(is.null(input$bbl))       # if missing BBL, return to avoid error
      return()
    lot <- bblRow() 
    
    if (is.null(input$material)) # if missing material, return to avoid error
      return()
    if (is.na(input$material))   # if missing material, return to avoid error
      return()
    
    btype <- typeidList[which(materialList == input$material)]
    id <- as.numeric(lot[btype])  # buildingtypeid
    if (is.null(id)) # if missing material, return to avoid error
      return()
    if (is.na(id))   # if missing material, return to avoid error
      return()
    qclasses <- as.vector(t(desc[which(desc$buildingtypeid == id), 2:7]))
    qualityOptions <- qclasses[!is.na(qclasses)]
    selectizeInput("quality", "Choose a construction quality:", 
                   choices = qualityOptions, selected = qualityOptions[2])
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
    return(get_TotalCost(BBL = input$bbl, Material = input$material, 
                  QualityClass = input$quality, BsmtType = input$basement,
                  PlutoTable = mn))
    
  }) 

  computeDamage <- reactive({
    pr <- get_PlutoRow(BBL = input$bbl, PlutoTable = mn)
    bclass <- get_BldgClass(BBL = input$bbl, PlutoTable = mn)

    curve <- get_DamageProbs(BldgClass = bclass, BsmtType = input$basement,
                    FloodType = pr$floodtype)
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
    bblRow()[displayColList]
  })
  
  # # Show the input values using an HTML table
  output$values <- renderTable({
    if(is.null(input$bbl))       # if missing BBL, return to avoid error
      return()
    if (is.null(input$material)) # if missing material, return to avoid error
      return()
    if (anyNA(input$material)) # if missing material, return to avoid error
      return()
    if (is.null(input$quality))   # if missing material, return to avoid error
      return()
    if (anyNA(input$quality)) # if missing material, return to avoid error
      return()
    if (is.null(input$basement))   # if missing material, return to avoid error
      return()

    inputValues()
  })
  
  output$show_totalcost <- renderPrint({
    if(is.null(input$bbl))       # if missing BBL, return to avoid error
      return()
    if (is.null(input$material)) # if missing material, return to avoid error
      return()
    # if (is.na(input$material)) # if missing material, return to avoid error
    #   return()
    # if (anyNA(input$material)) # if missing material, return to avoid error
    #   return()
    if (is.null(input$quality))   # if missing material, return to avoid error
      return()
    # if (anyNA(input$quality)) # if missing material, return to avoid error
    #   return()
    # if (is.null(input$basement))   # if missing material, return to avoid error
    #   return()
    
    computeCost()
  })
  
  output$show_damage <- renderPrint({
    if(is.null(input$bbl))       # if missing BBL, return to avoid error
      return()
    if (is.null(input$material)) # if missing material, return to avoid error
      return()
    # if (anyNA(input$material)) # if missing material, return to avoid error
      # return()
    if (is.null(input$quality))   # if missing material, return to avoid error
      return()
    # if (anyNA(input$quality)) # if missing material, return to avoid error
      # return()
    if (is.null(input$basement))   # if missing material, return to avoid error
      return()
    # if (is.null(computeCost())) # if missing material, return to avoid error
      # return()
    # if (anyNA(computeCost())) # if missing material, return to avoid error
      # return()
    computeDamage()
  })
  
})
