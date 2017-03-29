
get_PlutoRow <- function(BBL, PlutoTable){
        lotRow <- PlutoTable[which(PlutoTable$BBL == BBL), ]
        return(lotRow)  # returns a data.frame of 1 obs
}

# BBL is a number, Material is a string from materialList ("Default", "Wood", "Masonry", "Concrete", "Steel"), 
# get_Typeid() returns a number, which stands for BuildingTypeID in the cost table.
get_Typeid <- function(BBL, Material, PlutoTable){
        btype <- typeidList[which(materialList == Material)]
        # typeidList --- c("Undefined", "Wood",  "Masonry",  "Concrete", "Steel")
        typeid <- PlutoTable[which(PlutoTable$BBL == BBL), btype]
        return(typeid)
}

# BBL is a number, PlutoTable is a data.frame. 
# get_Shapeid() returns a number, which stands for BuildingShapeID in the cost table.
get_Shapeid  <- function(BBL, PlutoTable){
        shapeid <- PlutoTable[which(PlutoTable$BBL == BBL), "bldgshape_id"]
        return(shapeid)
}


# BBL is a number, PlutoTable is a data.frame. 
# get_BldgClass() returns a string, which stands for BldgClass in the damage table.
get_BldgClass  <- function(BBL, PlutoTable){
        bclass <- PlutoTable[which(PlutoTable$BBL == BBL), "BldgClass"]
        return(bclass)  
}

get_QualityClasses <- function(BldgTypeID){
        qclasses <- as.vector(t(desc[which(desc$buildingtypeid == BldgTypeID), qualityList]))
        return(qclasses[complete.cases(qclasses)])
}

# BldgTypeID is a number, BldgShapeID is a number, 
# CostType is a string in (FirstFloor", "SecondFloor&Up")
#*QualityClassNum is a integer represent class number: e.g. 1 for class 1
# get_FloorCoefs() returns a list of numbers of length 2, 
# the 1st number is intercept, the 2nd number is slope.
get_FloorCoefs <- function(BldgTypeID, BldgShapeID, CostType, QualityClassNum){
        name1 <- paste("class", as.character(QualityClassNum), ".intercept", sep = "")
        name2 <- paste("class", as.character(QualityClassNum), ".slope", sep = "")
        in.sl <- cost[cost$buildingtypeid == BldgTypeID &  cost$buildingshapeid == BldgShapeID & cost$costtype == CostType, 
                      c(name1,name2)]
        return(as.vector(t(in.sl)))
}


get_BsmtCoefs <- function(BldgTypeID){
        in.sl <- cost[cost$buildingtypeid == BldgTypeID & cost$costtype == "Basement", 
                      c("class1.intercept", "class1.slope")]
        return(as.vector(t(in.sl)))
}


# BldgClass is a string, 
#*BsmtType is a boolean value, of TRUE or FALSE, 
# FloodType is a string of "Fresh" or "Salt",
# get_DamageProbs() returns a list of percentages of length 29.
get_DamageProbs <- function(BldgClass, BsmtType, FloodType){
        bsmtString <- ifelse(BsmtType, "Yes", "No")
        probRow <- prob[prob$bldgclass == BldgClass & prob$bsmttype == bsmtString & 
                          prob$floodtype == FloodType, ]
        percs <- as.vector(t(probRow[, 5:33])) / 100
        return(percs)
}



# For a given CostType ("Basement", "FirstFloor", "SecondFloor&Up")
# Unit Cost (per square foot) = intercept + slope * [1 / sqft(Floor Area)]
# Total Cost ($) = Unit Cost * Floor Area


# BBL is an integer
# Material is a string from materialList ("Undefined", "Wood", "Masonry", "Concrete", "Steel"), 
# QualityClass is a string in desc table associated with buildingtypeid.
# BsmtType is a boolean value, of TRUE or FALSE, 
# PlutoTable is a data.frame of NYC PLUTO. 
get_TotalCost <- function(BBL, Material, QualityClassNum, BsmtType, PlutoTable){
  r <- PlutoTable[which(PlutoTable$BBL == BBL), ]
  if (!isTRUE(r$cost_calc)){
    return("The construction cost can not be calculated.")
  }
  
  typeid  <- get_Typeid(BBL, Material, PlutoTable)
  shapeid <- get_Shapeid(BBL, PlutoTable)
  print(paste("BuildingTypeID:",typeid))
  # extract basement cost coefficients\
  bsmt.coefs <- get_BsmtCoefs(typeid)
  # extract first floor cost coefficients
  firstcoefs <- get_FloorCoefs(BldgTypeID = typeid, BldgShapeID = shapeid,
                               CostType = "FirstFloor", 
                               QualityClassNum = QualityClassNum)
  bsmt.intercept  <- bsmt.coefs[1]
  bsmt.slope      <- bsmt.coefs[2]
  first.intercept <- firstcoefs[1]
  first.slope     <- firstcoefs[2]
  # conditionally extract/copy second floor cost coefficients
  if (typeid %in% secondList){
    secondcoefs <- get_FloorCoefs(BldgTypeID = typeid, BldgShapeID = shapeid,
                                  CostType = "SecondFloor&Up", 
                                  QualityClassNum = QualityClassNum)
    second.intercept <- secondcoefs[1]
    second.slope     <- secondcoefs[2]
  } else {
    # same as first floor coefficients
    second.intercept <- first.intercept
    second.slope     <- first.slope
  }
  
  if (!is.na(r$bsmtarea) & r$bsmtarea == 0){
    unit.bsmt   <- 0 
    unit.first  <- first.intercept + first.slope * r$nb_singlefloorarea^(-0.5)
    unit.second <- second.intercept + second.slope * r$nb_singlefloorarea^(-0.5)
    area.bsmt  <- 0
    area.floor <- r$nb_singlefloorarea
  } else if (!is.na(r$bsmtarea) & r$bsmtarea != 0){
    unit.bsmt   <- bsmt.intercept + bsmt.slope * r$bsmtarea^(-0.5)
    unit.first  <- first.intercept + first.slope * r$b_singlefloorarea^(-0.5)
    unit.second <- second.intercept + second.slope * r$b_singlefloorarea^(-0.5)
    area.bsmt  <- r$bsmtarea
    area.floor <- r$b_singlefloorarea
  } else{
    if (BsmtType){  # user choose: with basement
      unit.bsmt   <- bsmt.intercept + bsmt.slope * r$b_singlefloorarea^(-0.5)
      unit.first  <- first.intercept + first.slope * r$b_singlefloorarea^(-0.5)
      unit.second <- second.intercept + second.slope * r$b_singlefloorarea^(-0.5)
      area.bsmt  <- r$b_singlefloorarea
      area.floor <- r$b_singlefloorarea
    } else{  # user choose: no basement
      unit.bsmt   <- 0
      unit.first  <- first.intercept + first.slope * r$nb_singlefloorarea^(-0.5)
      unit.second <- second.intercept + second.slope * r$nb_singlefloorarea^(-0.5)
      area.bsmt  <- r$nb_singlefloorarea
      area.floor <- r$nb_singlefloorarea
    }
  }
  
  # total cost
  if (r$NumFloors == 0){
    total.cost <- unit.bsmt * area.bsmt
  } else if(r$NumFloors == 1){
    total.cost <- unit.bsmt * area.bsmt + unit.first * area.floor    
    
  } else{
    total.cost <- (unit.bsmt * area.bsmt + unit.first * area.floor + 
                     unit.second * (r$NumFloors - 1 ) * area.floor)
  }
  
  total.cost <- total.cost * r$mod
  # print(paste("unit.bsmt:", unit.bsmt, "area.bsmt:", area.bsmt,
  #             "unit.first:", unit.first, "unit.second:", unit.second,
  #             "area.floor:", area.floor))
  displayCosts <- data.frame(
    Name = c("Basement Unit Cost", 
             "Basement Area",
             "First Floor Unit Cost",
             "Second Floor & Up Unit Cost",
             "Single Floor Area"),
    
    Value = as.character(c(round(unit.bsmt,0), 
                           round(area.bsmt,0),
                           round(unit.first,0),
                           round(unit.second,0),
                           round(area.floor,0)
    )), 
    stringsAsFactors=FALSE)
  print(displayCosts)
  
  
  return(total.cost)
  
}


