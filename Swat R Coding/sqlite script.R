rm(list = ls())
#https://cran.r-project.org/web/packages/arsenal/vignettes/comparedf.html
#https://www.datacamp.com/community/tutorials/sqlite-in-r
#https://www.r-bloggers.com/2012/11/r-and-sqlite-part-1/


setwd("C:/Users/Brian/Downloads/")


library("RSQLite")

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname="swatplus_soils.sqlite")

## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- vector("list", length=length(tables))

## create a data.frame for each table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

x=data.frame(lDataFrames[[3]])

x= as.data.frame(x$seqn)


dbDisconnect(conn =   con)

write.csv(x, "SWAT_plus_Soils_MukeyIDs.csv")
library(arsenal)
rm(list=ls())

setwd("CF_90m_soils/")
cf90=read.csv("CF_90m_soils_mukeyedit_check.csv")
setwd("../")
DB=read.csv("SWAT_plus_Soils_MukeyIDs.csv")
DB$X=NULL
cf90$X=NULL

library(plyr)


soils=cf90

# returns all values of A$C that are NOT in B$C
missingmukeys= soils$CF90mSoils[!soils$CF90mSoils %in% DB$x.seqn]
missingmukeys = data.frame(missingmukeys)
summary(as.factor(missingmukeys$missingmukeys))
x = unique(missingmukeys$missingmukeys)
x

soils1 <- soils[which(soils$CF90mSoils== 2973893),]
write.csv(soils1, "cf90_probmukey_1.csv")                     

soils2 <- soils[which(soils$CF90mSoils== 3050330),]
write.csv(soils2, "cf90_probmukey_2.csv")                     

soils3 <- soils[which(soils$CF90mSoils== 142944),]
write.csv(soils3, "cf90_probmukey_3.csv")                     

soils4 <- soils[which(soils$CF90mSoils== 3050372),]
write.csv(soils4, "cf90_probmukey_4.csv")                     

soils5 <- soils[which(soils$CF90mSoils== 2557347),]
write.csv(soils5, "cf90_probmukey_5.csv")                     

soils6 <- soils[which(soils$CF90mSoils== 2538679),]
write.csv(soils6, "cf90_probmukey_6.csv")                     



c(2973893, 3050330, 142944, 3050372, 2557347, 2538679)



