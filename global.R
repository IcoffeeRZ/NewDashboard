#
################################### Test_New ###################################
#
cost <- read.csv("data/costco.csv", stringsAsFactors = F, strip.white = T)
mn   <- read.csv("data/mn_withdist.csv", stringsAsFactors = F, strip.white = T)
prob <- read.csv("data/newprob.csv", stringsAsFactors = F, strip.white = T)
desc <- read.csv("data/qualityclassdesc.csv", stringsAsFactors = F,
                 strip.white = T, na = "")

mn <- mn[order(mn$bbl),]    # reorder mn by bbl column
rownames(mn) <- 1:nrow(mn)  # rename rows after ordering

materialList   <- c("Default", "Wood", "Masonry", "Concrete", "Steel")
qualityList    <- c("Class1", "Class2", "Class3", "Class4", "Class5", "Class6")
displayColList <- c("zipcode", "address", "bldgclass","numfloors", 
                    "bldgarea", "bsmtarea")
matLogic       <- c("u", "w", "m", "c", "s")
floorLogic     <- c("basement", "firstfloor", "secondfloor")
typeidList     <- colnames(mn)[27:31]
costTypeList   <- c("Basement", "FirstFloor", "SecondFloor&Up")

bblDisplay <- test$bbl

#==============================================================================#
get_PlutoRow <- function(BBL, PlutoTable){
  rowNum <- which(PlutoTable$bbl == BBL)   
  lotRow <- PlutoTable[rowNum, ]
  return(lotRow)
}

# BBL is a number, Material is a string from 
#*materialList ("Undefined", "Wood", "Masonry", "Concrete", "Steel"), 
# PlutoTable is a data.frame. 
# -------
# get_Typeid() returns a number, which stands for BuildingTypeID 
# in the cost table.
get_Typeid <- function(BBL, Material, PlutoTable){
  rowNum <- which(PlutoTable$bbl == BBL)   
  lotRow <- PlutoTable[rowNum, ]
  btype <- typeidList[which(materialList == Material)]
  typeid <- as.numeric(lotRow[btype])  # buildingtypeid
  return(typeid)
}

# BBL is a number, PlutoTable is a data.frame. 
# -------
# get_Shapeid() returns a number, which stands for BuildingShapeID
# in the cost table.
get_Shapeid <- function(BBL, PlutoTable){
  rowNum <- which(PlutoTable$bbl == BBL)   
  lotRow <- PlutoTable[rowNum, ]
  shapeid <- as.numeric(lotRow["bldgshapeid"])  # buildingtypeid
  return(shapeid)
}

# BBL is a number, PlutoTable is a data.frame. 
# -------
# get_BldgClass() returns a string, which stands for BldgClass
# in the damage table.
get_BldgClass <- function(BBL, PlutoTable){
  rowNum <- which(PlutoTable$bbl == BBL)   
  lotRow <- PlutoTable[rowNum, ]
  bclass <- as.character(lotRow["bldgclass"])  # buildingtypeid
  return(bclass)  
}

get_QualityClasses <- function(BldgTypeID){
  qclasses <- as.vector(t(desc[which(desc$buildingtypeid == BldgTypeID), 2:7]))
  return(qclasses)
}

# BldgTypeID is a number, BldgShapeID is a number, 
# CostType is a string in costTypeList ("Basement", "FirstFloor", "SecondFloor&Up")
#*QualityClass is a string in desc table associated with buildingtypeid.
# -------
# get_FloorCoefs() returns a list of numbers of length 2, 
# the 1st number is intercept, the 2nd number is slope.
get_FloorCoefs <- function(BldgTypeID, BldgShapeID, CostType, QualityClass){
  costRow <- cost[cost$buildingtypeid == BldgTypeID & 
                  cost$buildingshapeid == BldgShapeID & 
                  cost$costtype == CostType, ]
  qualityClasses <- get_QualityClasses(BldgTypeID)
  classNum <- which(qualityClasses == QualityClass)
  ind1 <- 2*classNum + 2
  ind2 <- 2*classNum + 3
  in.sl <- as.vector(t(costRow[, ind1:ind2]))
  return(in.sl)
}

get_BsmtCoefs <- function(BldgTypeID){
  bsmtcostRow <- cost[cost$buildingtypeid == BldgTypeID & 
                            cost$costtype == "Basement", ]
  in.sl <- as.vector(t(bsmtcostRow[, 4:5]))
  return(in.sl)
}


# BldgClass is a string, 
#*BsmtType is a boolean value, of TRUE or FALSE, 
# FloodType is a string of "Fresh" or "Salt",
# -------
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


# BBL is a number, Material is a string from 
# Material is a string from materialList ("Undefined", "Wood", "Masonry", "Concrete", "Steel"), 
# QualityClass is a string in desc table associated with buildingtypeid.
# BsmtType is a boolean value, of TRUE or FALSE, 
# PlutoTable is a data.frame of NYC PLUTO. 
get_TotalCost <- function(BBL, Material, QualityClass, BsmtType, PlutoTable){
  r <- get_PlutoRow(BBL, PlutoTable)
  # total building area check
  if (is.na(r$bldgarea)){
    print("Building area is missing.")
    return(NA)
  } else if(r$bldgarea == 0){
    print("Building area is 0.")
    return(NA)
  }
  # number of buildings check
  if (r$numbldgs > 1){
    print("Number of buildings is larger than 1 or is 0.")
    return(NA)
  }
  # material check
  matCheck <- materialList[as.vector(t(r[matLogic]))]
  if (all(is.na(matCheck))){
    print("No material available. LandUse type is Vacant Land")
    return(NA)
  }
  
  typeid  <- get_Typeid(BBL, Material, PlutoTable)
  shapeid <- get_Shapeid(BBL, PlutoTable)
  print(paste("BuildingTypeID:",typeid))
  # extract basement cost coefficients
  bsmt.intercept <- get_BsmtCoefs(typeid)[1]
  bsmt.slope     <- get_BsmtCoefs(typeid)[2]
  # extract first floor cost coefficients
  firstcoefs <- get_FloorCoefs(BldgTypeID = typeid, BldgShapeID = shapeid,
                                    CostType = "FirstFloor", 
                                    QualityClass = QualityClass)
  first.intercept <- firstcoefs[1]
  first.slope     <- firstcoefs[2]
  # conditionally extract/copy second floor cost coefficients
  if (!is.na(r$secondfloor) &r$secondfloor){
    secondcoefs <- get_FloorCoefs(BldgTypeID = typeid, BldgShapeID = shapeid,
                                  CostType = "SecondFloor&Up", 
                                  QualityClass = QualityClass)
    second.intercept <- secondcoefs[1]
    second.slope     <- secondcoefs[2]
  } else {
    # same as first floor coefficients
    second.intercept <- first.intercept
    second.slope     <- first.slope
  }
  
  if (!is.na(r$bsmtarea) & r$bsmtarea == 0){
    unit.bsmt   <- 0 
    unit.first  <- first.intercept + first.slope * r$nb.singlefloorarea^(-0.5)
    unit.second <- second.intercept + second.slope * r$nb.singlefloorarea^(-0.5)
    area.bsmt  <- 0
    area.floor <- r$nb.singlefloorarea
  } else if (!is.na(r$bsmtarea) & r$bsmtarea != 0){
    unit.bsmt   <- bsmt.intercept + bsmt.slope * r$bsmtarea^(-0.5)
    unit.first  <- first.intercept + first.slope * r$b.singlefloorarea^(-0.5)
    unit.second <- second.intercept + second.slope * r$b.singlefloorarea^(-0.5)
    area.bsmt  <- r$bsmtarea
    area.floor <- r$b.singlefloorarea
  } else{
    if (BsmtType){  # user choose: with basement
      unit.bsmt   <- bsmt.intercept + bsmt.slope * r$b.singlefloorarea^(-0.5)
      unit.first  <- first.intercept + first.slope * r$b.singlefloorarea^(-0.5)
      unit.second <- second.intercept + second.slope * r$b.singlefloorarea^(-0.5)
      area.bsmt  <- r$b.singlefloorarea
      area.floor <- r$b.singlefloorarea
    } else{  # user choose: no basement
      unit.bsmt   <- 0
      unit.first  <- first.intercept + first.slope * r$nb.singlefloorarea^(-0.5)
      unit.second <- second.intercept + second.slope * r$nb.singlefloorarea^(-0.5)
      area.bsmt  <- r$nb.singlefloorarea
      area.floor <- r$nb.singlefloorarea
    }
  }
    
  # total cost
  if (r$numfloors == 0){
    total.cost <- unit.bsmt * area.bsmt
  } else if(r$numfloors == 1){
    total.cost <- unit.bsmt * area.bsmt + unit.first * area.floor    
  
  } else{
    total.cost <- (unit.bsmt * area.bsmt + unit.first * area.floor + 
                   unit.second * (r$numfloors - 1 ) * area.floor)
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
    
    Value = as.character(c(round(unit.bsmt,2), 
                           round(area.bsmt,2),
                           round(unit.first,2),
                           round(unit.second,2),
                           round(area.floor,2)
    )), 
    stringsAsFactors=FALSE)
  print(displayCosts)


  return(total.cost)
  
}

# # bldgarea has no missing values
# summary(mn$bldgarea)
# dim(mn[is.na(mn$bsmtarea) & is.na(mn$b.singlefloorarea) & is.na(mn$nb.singlefloorarea), ])
# length(mn$bldgarea[mn$bldgarea == 0])
# table(mn$bldgclass)
# # 62 bldgclass missing
# 
# dim(mn[is.na(mn$u.buildingtypeid) & is.na(mn$w.buildingtypeid) & 
#        is.na(mn$m.buildingtypeid) & is.na(mn$c.buildingtypeid) & 
#        is.na(mn$s.buildingtypeid), ])
# # 2596 tax lots with no buildingtypeid
# dim(mn[!(mn$u | mn$w | mn$m | mn$c | mn$s), ])
# # 2596 tax lots with no material
# 
# dim(mn[!is.na(mn$bsmtarea) & mn$numfloors == 0 & mn$bsmtarea != 0, ])


#-------------------------------------------------------------------------------
# Subset BBL by random draw

allBBL <- sample(mn$bbl, size = 100, replace = FALSE)
















































