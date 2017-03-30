cost <- read.csv("data/costco.csv", stringsAsFactors = F, strip.white = T)
mn   <- read.csv("data/mn_withdist.csv", stringsAsFactors = F, strip.white = T)
prob <- read.csv("data/newprob.csv", stringsAsFactors = F, strip.white = T)
desc <- read.csv("data/qualityclassdesc.csv", stringsAsFactors = F,
                 strip.white = T, na = "")
pluto.classdesc <- read.csv("data/pluto_bldgclass_desc.csv", stringsAsFactors = F, strip.white = T)

# ITMN <- read.csv("data/to_IT_MN.csv", stringsAsFactors = F, strip.white = T)
cleanMN <- read.csv("data/clean_MN.csv", stringsAsFactors = F, strip.white = T)


mn <- mn[order(mn$BBL),]    # reorder mn by BBL column
rownames(mn) <- 1:nrow(mn)  # rename rows after ordering

materialList   <- c("Default", "Wood", "Masonry", "Concrete", "Steel")
qualityList    <- c("class1", "class2", "class3", "class4", "class5", "class6")
displayColList <- c("ZipCode", "Address", "BldgClass","NumFloors", 
                    "BldgArea", "bsmtarea")
# matLogic       <- c("Undefined", "Wood", "Masonry", "Concrete", "Steel")
floorLogic     <- c("basement", "firstfloor", "secondfloor")
typeidList     <- c("Undefined", "Wood", "Masonry", "Concrete", "Steel")
costTypeList   <- c("Basement", "FirstFloor", "SecondFloor&Up")


# bblDisplay <- test$BBL



#-------------------------------------------------------------------------------
# Subset BBL by random draw

# allBBL <- sample(mn$BBL, size = 100, replace = FALSE)

ctSummary <- table(cost$buildingtypeid, cost$costtype)
costtype.summary <- as.data.frame(as.matrix.data.frame(ctSummary))
colnames(costtype.summary) <- colnames(ctSummary)
costtype.summary$buildingtypeid <- rownames(ctSummary)
costtype.summary <- costtype.summary[, c(4,1,2,3)]

secondList <- costtype.summary$buildingtypeid[costtype.summary$`SecondFloor&Up` != 0]

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