


###TEST get_Typeid()
# BBLs are from mn_withdist.csv

get_Typeid(1000390001, "Wood", mn)
# [1] 32
get_Typeid(1000390001, "Masonry", mn)
# [1] 30
get_Typeid(1022150387, "Masonry", mn)
# [1] NA
get_Typeid(1022150387, "Default", mn)
# [1] 1


###TEST get_Shapeid()

get_Shapeid(1015570008,mn)
# [1] 31


###TEST get_BldgClass()

# BBLs are from mn_withdist.csv
get_BldgClass(1022150387, mn)
# [1] "A5"

get_BldgClass(1000390001, mn)
# [1] "O6"



###TEST get_QualityClasses()

get_QualityClasses(1)
# [1] "Luxury (for buildings with 10+ corners)"     
# [2] "Semi-Luxury (for buildings with 10+ corners)"
# [3] "Best Std."                                   
# [4] "Good Std."                                   
# [5] "Average Std."                                
# [6] "Minimum Std." 


###TEST get_FloorCoefs()

get_FloorCoefs(1, 1, "FirstFloor", 1)
get_FloorCoefs(1, 1, "FirstFloor", 1)[1]
#    class1.intercept class1.slope
# 1         210.8883     6865.586

# [1]  210.8883 6865.5861


###TEST get_BsmtCoefs()

get_BsmtCoefs(1)
#      class1.intercept class1.slope
# 142         3.380541     886.4433


###TEST get_DamageProbs()

get_DamageProbs("A2", TRUE, "Fresh")
# [1] 0.012 0.024 0.060 0.096 0.120 0.150 0.190 0.240 0.260 0.290
# [11] 0.310 0.320 0.370 0.420 0.450 0.460 0.470 0.490 0.500 0.510
# [21] 0.520 0.540 0.550 0.560 0.570 0.590 0.600 0.610 0.630




get_TotalCost(BBL = 1000020002, Material = "Wood", QualityClassNum = 1, 
              BsmtType = TRUE, PlutoTable = mn)


# [1] "BuildingTypeID: 406"
#                          Name Value
# 1          Basement Unit Cost   137
# 2               Basement Area 26366
# 3       First Floor Unit Cost   291
# 4 Second Floor & Up Unit Cost   189
# 5           Single Floor Area 26366
# [1] 37376113


which(get_QualityClasses(get_Typeid(1000020002, "Wood", mn)) == "Good")

get_Typeid(1000020002, "Wood", mn)
test = get_QualityClasses(get_Typeid(1000020002, "Wood", mn))

which(test == "Wood", arr.ind = T)





get_FloorCoefs(BldgTypeID = 406, BldgShapeID = 10, 
               CostType = "FirstFloor", QualityClassNum = 1)

  
get_TotalCost(BBL = 1014650009, Material = "Default", QualityClassNum = 1, 
              BsmtType = TRUE, PlutoTable = mn)  

  
get_DamageProbs("A0", TRUE, "Fresh")  
  
ggplot(seq_along(seq(-4,24)),get_DamageProbs("A0", TRUE, "Fresh"))  
qplot(seq(-4,24), 100*get_DamageProbs("A0", TRUE, "Fresh"), ylab = "Percentage (%)", xlab ="Water Depth (ft)", geom = c("point", "smooth")) + geom_line()  




sprintf("%s costed $ %3.2f ", stuff, price)
















  