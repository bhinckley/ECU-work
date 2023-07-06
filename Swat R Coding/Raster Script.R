rm(list=ls())
setwd("C:/Users/Brian/Downloads/CF_90m_soils/")

install.packages("rgdal")
install.packages("raster")

library(rgdal)
library(raster)

ninety=raster("CF90mSoils.tif")
#thirty=raster("soil_rasterize_clip_30m.tif")

n1=as.data.frame(ninety, xy=T)
#t1=as.data.frame(thirty, xy=T)

n2=na.omit(n1)

write.csv(n2, "CF_90m_soils_mukeyedit_check.csv")




# Current MUKEYs To replace: 2973893 3050330  142944 3050372 2557347 2538679

#1  #replace 2973893 with:
#2  #replace 3050330 with:
#3  #replace 142944 with:
#4  #replace 3050372 with:
#5  #replace 2557347 with:
#6  #replace 2538679 with:

library(raster)
ninety <- reclassify(ninety, cbind(2973893, 2973922))
ninety <- reclassify(ninety, cbind(3050330, 3050346))
ninety <- reclassify(ninety, cbind(142944, 142917))
ninety <- reclassify(ninety, cbind(3050372, 3050374))
ninety <- reclassify(ninety, cbind(2557347, 2557360))
ninety <- reclassify(ninety, cbind(2538679, 2538573))

writeRaster(ninety, "CF90mSoils_RMukeyEdit.tif",format="GTiff", overwrite=TRUE)















#read in soils tabular raster
#x = read.csv("CF_500mSoils_UNEDITED.csv")
#x$X=NULL
#p = read.csv("CF500_SSURGO_MUKEY_NA_comparison.csv")
#tab=summary(as.factor(p$mukeyCF500))
#tab
#using table output, manually subset out problem mukeys from unedited

#x2 = x[x$CF_500m_soils != 142944,  ]
#x2 = x2[x2$CF_500m_soils != 2538679, ]
#x2 = x2[x2$CF_500m_soils != 2557347, ]
#x2 = x2[x2$CF_500m_soils != 3050330 , ]
#x2 = x2[x2$CF_500m_soils != 3050372  , ]


#check summary of unedited, make sure only lose the 735 specific problem rows
#summary(x)
#summary(x2)

#nrow(x)-nrow(x2)

#write out problem mukey-omitted soils raster. Read into QGIS and Rasterize
#write.csv(x2, "CF500_soils_missingMUKEY_Removed.csv")



