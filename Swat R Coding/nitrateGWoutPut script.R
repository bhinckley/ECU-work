rm(list=ls())

setwd("C:/Users/Brian/Desktop/GW Testing/")
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(hydroGOF)


no3_sums=read.csv("pp.csv")

NO3Sims =read.csv("filteredAQUNO3.csv")
#save ulfiltered NO3Sims before it gets filtered down
NO3Sims1=NO3Sims
NO3Sims1$Date<-as.Date(NO3Sims1$date,format='%Y-%m-%d')


Parameters = read.csv("par_set_vis_CFTarheel.csv")

obsNO3 = read.csv("obsGWNO3_cleanedFINAL.csv") # the 1.6 value is our problem value I have been removing
summary(obsNO3)
obsNO3=obsNO3[c(1:7,9:15),]
obsNO3 = obsNO3 %>% separate(date, c("month", "day","year"))

obsNO3$date2 = paste(obsNO3$year, obsNO3$month, obsNO3$day, sep = "-")
obsNO3$date3<-as.Date(obsNO3$date2,format='%Y-%m-%d')
obsNO3$month = obsNO3$date3
obsNO3$day=NULL
obsNO3$year=NULL
obsNO3$date2=NULL
obsNO3$date=NULL
obsNO3$date3=NULL

obsNO3 = rename(obsNO3,  date = "month" )



#fit no3 daily sums to parameters
no3_sums$index=row.names(no3_sums)
no3_sums$index=as.character(no3_sums$index)
Parameters = rename(Parameters,  index = "X" )
Parameters = rename(Parameters,  FlowNSE = "NSE" )
Parameters$index=as.character(Parameters$index)
no3_sums = rename(no3_sums,  NO3Sum = "x" )
no3_sums = rename(no3_sums,  SimID = "X" )

#fit no3 sums to Model Parameters indexed results
Parameters = merge(Parameters, no3_sums, all.x = TRUE)
Parameters=Parameters[order(Parameters$SimID),]



NO3Sims$X=NULL
sims = NO3Sims[,2:length(NO3Sims)]
NO3Sims = NO3Sims$date
#subset model no3 simulations to parameter SimID to adjust for missing runs
sims =sims %>% select(Parameters$SimID)
sims <- sims %>%   
  select(sort(names(.)))
NO3Sims=data.frame(NO3Sims)
NO3Sims=cbind(NO3Sims, sims)
NO3Sims$temp1<-as.Date(NO3Sims$NO3Sims,format='%Y-%m-%d')

NO3Sims$NO3Sims = NO3Sims$temp1
NO3Sims$temp1 = NULL
NO3Sims = rename(NO3Sims,  date = "NO3Sims" )

#subset NO3 sims to obs NO3 dates
NO3Sims = merge(obsNO3, NO3Sims, all.x = TRUE)
NO3Sims$NO3=NULL
NO3Sims$date3=NULL
sims = NO3Sims[,2:length(NO3Sims)]



#Calculate NSE and R^2
run_sel_sub = NO3Sims[, c(2:ncol(NO3Sims))]
no3SimMatrix=run_sel_sub
no3OBSMatrix = run_sel_sub
no3OBSMatrix[, c(1:ncol(no3OBSMatrix))] = obsNO3$NO3

#Get NSE scores, filter out threshold data
NO3_STATS = gof(sim =no3SimMatrix, obs = no3OBSMatrix)
#grab NSE and R2 columns
NO3_STATS= NO3_STATS[c(9,17),]
Parameters$NO3_NSE = NO3_STATS[1,]
Parameters$NO3_R2 = NO3_STATS[2,]

#### Grab top 5 NO3 NSE, sort by best flow NSE,grab sims from 
Parameters1=Parameters[order(Parameters$NO3_NSE, decreasing = TRUE),]
#Top 5
#Parameters1=Parameters1[c(1:5),]
#Best Flow
Parameters1=Parameters1[1,]
###
Parameters1=Parameters1[order(Parameters1$FlowNSE, decreasing = TRUE),]
sims_sub =sims %>% select(Parameters1$SimID)
NO3sims_sub = cbind(NO3Sims$date,sims_sub)
NO3sims_sub = rename(NO3sims_sub,  date = "NO3Sims$date" )
###

q_plot <- obsNO3 %>% 
  rename(obsNO3 = NO3) %>% # Rename the discharge columnt to q_obs
 # filter(year(date) %in% 2016:2018) %>% # Filter for years between 2003 and 2012
  left_join(., NO3sims_sub, by = 'date') %>% # Join with the q_plus table by date
  #left_join(., q_2012, by = 'date') %>% # Join with the q_plus table by date
  pivot_longer(., cols = -date, names_to = 'variable', values_to = 'NO3') # Make a long table for plotting

#Create jpeg name and size
jpeg(file="TarHeel_GWNO3_BestFlow_NO3NSE.jpeg", width = 1600, height = 800, res = 400, quality = 100)

#set line type to solid
lineset =colnames(NO3Sims)
lineset = replace(lineset, 1:length(lineset), "solid")
plot_colors = lineset
#Top 5, comment out next line for best flow
#plot_colors = replace(plot_colors, 3:length(plot_colors), "red") # rest of the top 5 NO3 NSE
plot_colors = replace(plot_colors, 2, "green") #top 5 NO3 NSE with best flow NSE score
plot_colors = replace(plot_colors, 1, "black") #Obs GW no3


ggplot(data = q_plot) +
  geom_line(aes(x = date, y = NO3, col = variable, lty = variable)) +
  scale_color_manual(values = plot_colors) +
  scale_linetype_manual(values = lineset) + 
  theme(legend.position="none")


dev.off()

###R2 plot for Sim vs obs GW NO3, Use this line if plotting best NO3 model


#### Grab top 5 NO3 NSE, sort by best flow NSE,grab sims from 
Parameters1=Parameters[order(Parameters$NO3_NSE, decreasing = TRUE),]
#Top 5
#Parameters1=Parameters1[c(1:5),]
#Best Flow
Parameters1=Parameters1[1,]
###
sims_sub =sims %>% select(Parameters1$SimID)
NO3sims_sub = cbind(NO3Sims$date,sims_sub)
NO3sims_sub = rename(NO3sims_sub,  date = "NO3Sims$date" )
fit = as.numeric(Parameters1[,24])
R2plot = cbind(obsNO3, NO3sims_sub[,2])
R2plot = rename(R2plot,  SimNO3 = "NO3sims_sub[, 2]" )
R2plot = rename(R2plot,  ObsNO3 = "NO3" )

jpeg(file="TarHeel_GWNO3_best_R2.jpeg", width = 1000, height = 800, res = 300, quality = 80)

p<-ggplot(R2plot, aes(x=ObsNO3, y=SimNO3)) + 
  geom_point(size=5, shape=1)+
  geom_smooth(method=lm, se=FALSE)

p + labs(title = "Best Groundwater NO3 Model R2", subtitle = fit)

dev.off()

####Read in full GW NO3 simulated period, plot best one, then plot all
fullNO3 =read.csv("CF_Chin_UplandAQU_NO3.csv")
fullNO3$X=NULL

fullNO3$date<-as.Date(fullNO3$date,format='%Y-%m-%d')
fullsims = fullNO3[,2:length(fullNO3)] 

Parameters1=Parameters[order(Parameters$NO3_NSE, decreasing = TRUE),]
Parameters1=Parameters1[1,]


fullsims_sub =fullsims %>% select(Parameters1$SimID)
fullsims_sub = cbind(fullNO3$date,fullsims_sub)
fullsims_sub = rename(fullsims_sub,  date = "fullNO3$date" )
temp = colnames(fullsims_sub)
fullsims_sub = rename(fullsims_sub,  Best_GWNO3_Sim = temp[2] )
#plot full No3 record for 02-18 best GW NO3
jpeg(file="TarHeel_GWNO3_FullSimPeriod.jpeg", width = 1000, height = 800, res = 300, quality = 80)

ggplot(data = fullsims_sub) +
  geom_line(aes(x = date, y = Best_GWNO3_Sim)) +
  theme(legend.position="none")

dev.off()

#Full record of sims plots

Parameters1=Parameters[order(Parameters$NO3_NSE, decreasing = TRUE),]
###
fullsims_sub =fullsims %>% select(Parameters1$SimID)
fullsims_sub = cbind(fullNO3$date,fullsims_sub)
fullsims_sub = rename(fullsims_sub,  date = "fullNO3$date" )


NO3_plot <- fullsims_sub %>% 
 # rename(obsNO3 = NO3) %>% # Rename the discharge columnt to q_obs
  # filter(year(date) %in% 2016:2018) %>% # Filter for years between 2003 and 2012
#  left_join(., NO3sims_sub, by = 'date') %>% # Join with the q_plus table by date
  #left_join(., q_2012, by = 'date') %>% # Join with the q_plus table by date
  pivot_longer(., cols = -date, names_to = 'variable', values_to = 'NO3') # Make a long table for plotting

jpeg(file="TarHeel_GWNO3_FullSimPeriod_AllSimulations.jpeg", width = 1600, height = 800, res = 400, quality = 100)

lineset =colnames(fullsims)
lineset = replace(lineset, 1:length(lineset), "solid")
plot_colors = lineset
plot_colors = replace(plot_colors, 1, "red")
plot_colors = replace(plot_colors, 2:length(plot_colors), "black") 

ggplot(data = NO3_plot) +
  geom_line(aes(x = date, y = NO3, col = variable, lty = variable)) +
  scale_color_manual(values = plot_colors) +
  scale_linetype_manual(values = lineset) + 
  theme(legend.position="none")


dev.off()

#plot flows
flows = read.csv("TarheelSurfaceFlowOut.csv")
flows$X=NULL

flows$date<-as.Date(flows$date,format='%Y-%m-%d')
flowsims = flows[,2:length(flows)] 

Parameters1=Parameters[order(Parameters$NO3_NSE, decreasing = TRUE),]
Parameters1=Parameters1[c(1:5),]
Parameters1=Parameters1[order(Parameters1$FlowNSE, decreasing = TRUE),]
Parameters1=Parameters1[1,]


fullsims_sub =flowsims %>% select(Parameters1$SimID)
fullsims_sub = cbind(flows$date,fullsims_sub)
fullsims_sub = rename(fullsims_sub,  date = "flows$date" )
temp = colnames(fullsims_sub)
fullsims_sub = rename(fullsims_sub,  Best_SurfaceFLow_NSE = temp[2] )

#Plot best Surface flow NSE in top 5 GW NO3 NSE
jpeg(file="TarHeel_SurfaceFlow_FullSimPeriod.jpeg", width = 1000, height = 800, res = 300, quality = 80)

ggplot(data = fullsims_sub) +
  geom_line(aes(x = date, y = Best_SurfaceFLow_NSE)) +
  theme(legend.position="none")

dev.off()
