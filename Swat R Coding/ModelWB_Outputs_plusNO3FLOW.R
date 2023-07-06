rm(list=ls())
setwd("E:/Cf_project_90m/obs data/")



#library(plyr)
library(hydroGOF)
library(SWATplusR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tibble)
library(lhs)

#set up the path to have no blanks
#highflow = 600000 m^3, 1000 NO3
#midflow = 300000 m^3, 10000 NO3
path = 'E:/Cf_project_90m/Swat projects/BermAgCornWheatSplits/BermAgCornWheatSplits/Scenarios/BermAgspillway50percUP/TxtInOut/'



no3_obs=read.csv("NE_CF_obsNO3.csv")


no3_obs = no3_obs %>% separate(Date, c("month", "day","year"))

no3_obs$date2 = paste(no3_obs$year, no3_obs$month, no3_obs$day, sep = "-")
no3_obs$date3<-as.Date(no3_obs$date2,format='%Y-%m-%d')

no3_obs$month = NULL
no3_obs$day= NULL
no3_obs$year= NULL
no3_obs$date2= NULL
no3_obs$date= no3_obs$date3
no3_obs$date3= NULL
no3_obs$d2=no3_obs$NO3
no3_obs$NO3=NULL
no3_obs$no3=no3_obs$d2
no3_obs$d2 = NULL



n <- 120
#locked perco, try surlag.bsn too
par_set <- tibble(
  'cn2.hru | change = abschg' = c(-5,5),
  'cn3_swf.hru | change = abschg' = c(.8,.9),
 # 'chn.rte | change = abschg' = c(.05,.25),
#  'wd_rto.rte | change = absval' = c(.5,20),
#  'lat_len.hru | change = absval' = c(22, 40),
  'canmx.hru | change = abschg' = c(-4,0),
  'latq_co.hru | change = pctchg' = c(-30,-20),
  'perco.hru | change = pctchg' = c(-10,10),
  'epco.hru | change = pctchg' = c(20,30),
  'esco.hru | change = pctchg' = c(30,40),
#  'k.sol | change = pctchg' = c(-10,50),
  'awc.sol | change = pctchg' = c(30,60),
    'ovn.hru | change = pctchg' = c(8,10),
  'flo_min.aqu | change = absval' = c(.5,10),
  'alpha.aqu | change = absval' = c(.40,.50),
  #    'gwflow_lte.hlt | change = absval' = c(0,10),
  #  'gw_lte.hlt | change = absval' = c(8000,10000),
  #  'vol.res | change = absval' = c(10,100),
  #  'orgn.res | change = absval' = c(0,1),
  #  'no3.res | change = absval' = c(0,1),
  #  'no2.res | change = absval' = c(0,1),
  #  'esa.res | change = absval' = c(0,3000),
  #  'evol.res | change = absval' = c(15,3000),
  #  'psa.res | change = absval' = c(10,100),
  #  'pvol.res | change = absval' = c(0,1),
  #  'k_res.res | change = absval' = c(0,1),
  #    'sw_lte.hlt | change = absval' = c(0,1),
  #'bf_max.aqu | change = absval' = c(.1,2),
  #    'revap_min.aqu | change = absval' = c(0,10),
  'revap_co.aqu | change = absval' = c(0.13,.14),
  'deep_seep.aqu | change = absval' = c(0.3,0.4),
  'nperco.bsn | change = absval' = c(0.1,.4),
  'cdn.bsn | change = absval' = c(0.5,1.2),
  'sdnco.bsn | change = absval' = c(0.1,.7),
  'n_updis.bsn | change = absval' = c(40,80),
  #'evlai.bsn | change = absval' = c(0,10),
  #'surlag.bsn | change = absval' = c(4,16),
  #'rs1.swq | change = absval' = c(.15,1.82),
  #'rs3.swq | change = absval' = c(.001,.1),
  #'rs4.swq | change = absval' = c(.001,.1),
  #'rk1.swq | change = absval' = c(.02,3.4),
  #'rk2.swq | change = absval' = c(0,100),
  #'rk3.swq | change = absval' = c(-0.36,.36),
  #'rk4.swq | change = absval' = c(0,100),
  #'bc1.swq | change = absval' = c(0.1,1),
  #'bc2.swq | change = absval' = c(.2,2),
  #'bc3.swq | change = absval' = c(.2,.4),
  #'algae.swq | change = absval' = c(0,200),
  #'organicn.swq | change = absval' = c(0,100),
  #'ammonian.swq | change = absval' = c(0,50),
  #'nitriten.swq | change = absval' = c(0,100),
  #    'msk_co1.bsn | change = absval' = c(0,10),
  #    'msk_co2.bsn | change = absval' = c(0,10),
  #    'msk_x.bsn | change = absval' = c(0,.3)
  #    'lat_orgn.hru | change = absval' = c(0,200)
  
)
par_set


set.seed(1111)
k=ncol(par_set)
Y <- data.frame(randomLHS(n, k))
Y[,1] <- qunif(Y[,1], min(par_set[1]), max(par_set[1]))
Y[,2] <- qunif(Y[,2], min(par_set[2]), max(par_set[2]))
Y[,3] <- qunif(Y[,3], min(par_set[3]), max(par_set[3]))
Y[,4] <- qunif(Y[,4], min(par_set[4]), max(par_set[4]))
Y[,5] <- qunif(Y[,5], min(par_set[5]), max(par_set[5]))
Y[,6] <- qunif(Y[,6], min(par_set[6]), max(par_set[6]))
Y[,7] <- qunif(Y[,7], min(par_set[7]), max(par_set[7]))
Y[,8] <- qunif(Y[,8], min(par_set[8]), max(par_set[8]))
Y[,9] <- qunif(Y[,9], min(par_set[9]), max(par_set[9]))
Y[,10] <- qunif(Y[,10], min(par_set[10]), max(par_set[10]))
Y[,11] <- qunif(Y[,11], min(par_set[11]), max(par_set[11]))
Y[,12] <- qunif(Y[,12], min(par_set[12]), max(par_set[12]))
Y[,13] <- qunif(Y[,13], min(par_set[13]), max(par_set[13]))
Y[,14] <- qunif(Y[,14], min(par_set[14]), max(par_set[14]))
Y[,15] <- qunif(Y[,15], min(par_set[15]), max(par_set[15]))
Y[,16] <- qunif(Y[,16], min(par_set[16]), max(par_set[16]))
Y[,17] <- qunif(Y[,17], min(par_set[17]), max(par_set[17]))
#Y[,18] <- qunif(Y[,18], min(par_set[18]), max(par_set[18]))
#Y[,19] <- qunif(Y[,19], min(par_set[19]), max(par_set[19]))
#Y[,20] <- qunif(Y[,20], min(par_set[20]), max(par_set[20]))
#Y[,21] <- qunif(Y[,21], min(par_set[21]), max(par_set[21]))
#Y[,22] <- qunif(Y[,22], min(par_set[22]), max(par_set[22]))
#Y[,23] <- qunif(Y[,23], min(par_set[23]), max(par_set[23]))
#Y[,24] <- qunif(Y[,24], min(par_set[24]), max(par_set[24]))
#Y[,25] <- qunif(Y[,25], min(par_set[25]), max(par_set[25]))
#Y[,26] <- qunif(Y[,26], min(par_set[26]), max(par_set[26]))
#Y[,27] <- qunif(Y[,27], min(par_set[27]), max(par_set[27]))
#Y[,28] <- qunif(Y[,28], min(par_set[28]), max(par_set[28]))
#Y[,29] <- qunif(Y[,29], min(par_set[29]), max(par_set[29]))
#Y[,30] <- qunif(Y[,30], min(par_set[30]), max(par_set[30]))
#Y[,31] <- qunif(Y[,31], min(par_set[31]), max(par_set[31]))
#Y[,32] <- qunif(Y[,32], min(par_set[32]), max(par_set[32]))
#Y[,33] <- qunif(Y[,33], min(par_set[33]), max(par_set[33]))
#Y[,34] <- qunif(Y[,34], min(par_set[34]), max(par_set[34]))
#Y[,35] <- qunif(Y[,35], min(par_set[35]), max(par_set[35]))
#Y[,36] <- qunif(Y[,36], min(par_set[36]), max(par_set[36]))


colnames(Y) = colnames(par_set)
par_set = Y
#matrix check
#pairs(par_set, pch=4)

q_simn <- run_swatplus(project_path = path, 
                       output = list(flow = define_output(file = 'channel_sd',
                                                          variable = 'flo_out',
                                                          unit = 44),
                                     no3 = define_output(file = "channel_sd",
                                                         variable = "no3_out",
                                                         unit = 44),
                                     GW = define_output(file = "basin_aqu",    #Basin-wide aqu flow into channels (mm H2O)
                                                         variable = "flo_cha",
                                                         unit = 1),
                                     no3_aquload = define_output(file = "basin_aqu",
                                                                  variable = "no3gw", #basin wide Nitrate loading to reach from aqu (kg N/ha)
                                                                  unit = 1),
                                     crop_yield = define_output(file = "basin_pw",
                                                                 variable = "yield", #basin-wide Crop yield (kg/ha)
                                                                 unit = 1),
                                     RunoffNO3loss = define_output(file = "basin_ls", #basin wide NO3 transported in runoff (kg N/ha)
                                                                 variable = "surqno3",
                                                                 unit = 1),
                                     Fert = define_output(file = "basin_nb",
                                                                    variable = "fertn",   #basin wide nitrogen fertilizer applied (kg N/ha)
                                                                    unit = 1),
                                     chan_runoff = define_output(file = "basin_wb", #basin wide runoff flowing into channels (mm H2O)
                                                          variable = "surq_cha",
                                                          unit = 1),
                                     latq = define_output(file = "basin_wb",
                                                            variable = "latq_cha", #basin wide lateral flow into channels (mm H2O)
                                                            unit = 1),
                                     Perc = define_output(file = "basin_wb",
                                                          variable = "perc", #basin wide perc into vadose zone (mm H2O)
                                                          unit = 1),
                                     Chinqchan_runoff = define_output(file = "hru_wb", #basin wide runoff flowing into channels (mm H2O)
                                                                 variable = "surq_cha",
                                                                 unit = 660),
                                     Chinqlatq = define_output(file = "hru_wb",
                                                          variable = "latq_cha", #basin wide lateral flow into channels (mm H2O)
                                                          unit = 660),
                                     ChinqGW = define_output(file = "aquifer",    #Basin-wide aqu flow into channels (mm H2O)
                                                        variable = "flo_cha",
                                                        unit = 31),
                                     ChinqPerc = define_output(file = "hru_wb",    #Basin-wide aqu flow into channels (mm H2O)
                                                             variable = "perc",
                                                             unit = 660)
                                     ),
                       
                       parameter = par_set,
                       n_thread = 10)
###NO3 from aqu2reach

AQUNO3toREACH = q_simn$simulation$no3_aquload
AQUNO3toREACH = AQUNO3toREACH[1097:2191,]
AQUNO3toREACH1 =colMeans(AQUNO3toREACH[,2:length(AQUNO3toREACH)])
tempAQU=colnames(AQUNO3toREACH[,2:length(AQUNO3toREACH)])
tempAQU=data.frame(tempAQU)
tempAQU = row.names(tempAQU)
tempAQU=data.frame(tempAQU)
AQUNO3toREACH1 = data.frame(AQUNO3toREACH1)
AQUNO3toREACH1 = cbind(tempAQU$tempAQU, AQUNO3toREACH1)
AQUNO3toREACH1 = rename(AQUNO3toREACH1,  Index = `tempAQU$tempAQU` )


#####AQU Flow Into Channels
AQU_FLOW2Chan = q_simn$simulation$GW
AQU_FLOW2Chan = AQU_FLOW2Chan[1097:2191,]
AQU_FLOW2Chan1 =colMeans(AQU_FLOW2Chan[,2:length(AQU_FLOW2Chan)])
tempAQU=colnames(AQU_FLOW2Chan[,2:length(AQU_FLOW2Chan)])
tempAQU=data.frame(tempAQU)
tempAQU = row.names(tempAQU)
tempAQU=data.frame(tempAQU)
AQU_FLOW2Chan1 = data.frame(AQU_FLOW2Chan1)
AQU_FLOW2Chan1 = cbind(tempAQU$tempAQU, AQU_FLOW2Chan1)
AQU_FLOW2Chan1 = rename(AQU_FLOW2Chan1,  index = `tempAQU$tempAQU` )

#####Crop Yield
CropYield = q_simn$simulation$crop_yield
CropYield = CropYield[1097:2191,]
CropYield1 =colMeans(CropYield[,2:length(CropYield)])
tempAQU=colnames(CropYield[,2:length(CropYield)])
tempAQU=data.frame(tempAQU)
tempAQU = row.names(tempAQU)
tempAQU=data.frame(tempAQU)
CropYield1 = data.frame(CropYield1)
CropYield1 = cbind(tempAQU$tempAQU, CropYield1)
CropYield1 = rename(CropYield1,  index = `tempAQU$tempAQU` )

#####Runoff NO3 Los
runoffNO3LOSS = q_simn$simulation$RunoffNO3loss
runoffNO3LOSS = runoffNO3LOSS[1097:2191,]
runoffNO3LOSS1 =colMeans(runoffNO3LOSS[,2:length(runoffNO3LOSS)])
tempAQU=colnames(runoffNO3LOSS[,2:length(runoffNO3LOSS)])
tempAQU=data.frame(tempAQU)
tempAQU = row.names(tempAQU)
tempAQU=data.frame(tempAQU)
runoffNO3LOSS1 = data.frame(runoffNO3LOSS1)
runoffNO3LOSS1 = cbind(tempAQU$tempAQU, runoffNO3LOSS1)
runoffNO3LOSS1 = rename(runoffNO3LOSS1,  index = `tempAQU$tempAQU` )


#Fert Applied
Fert = q_simn$simulation$Fert
Fert = Fert[1097:2191,]
Fert1 =colMeans(Fert[,2:length(Fert)])
tempAQU=colnames(Fert[,2:length(Fert)])
tempAQU=data.frame(tempAQU)
tempAQU = row.names(tempAQU)
tempAQU=data.frame(tempAQU)
Fert1 = data.frame(Fert1)
Fert1 = cbind(tempAQU$tempAQU, Fert1)
Fert1 = rename(Fert1,  Index = `tempAQU$tempAQU` )

#basin runoff into channels
chan_runoff = q_simn$simulation$chan_runoff
chan_runoff = chan_runoff[1097:2191,]
chan_runoff1 =colMeans(chan_runoff[,2:length(chan_runoff)])
tempAQU=colnames(chan_runoff[,2:length(chan_runoff)])
tempAQU=data.frame(tempAQU)
tempAQU = row.names(tempAQU)
tempAQU=data.frame(tempAQU)
chan_runoff1 = data.frame(chan_runoff1)
chan_runoff1 = cbind(tempAQU$tempAQU, chan_runoff1)
chan_runoff1 = rename(chan_runoff1,  Index = `tempAQU$tempAQU` )

#basin Lat flow into channels
latq = q_simn$simulation$latq
latq = latq[1097:2191,]
latq1 =colMeans(latq[,2:length(latq)])
tempAQU=colnames(latq[,2:length(latq)])
tempAQU=data.frame(tempAQU)
tempAQU = row.names(tempAQU)
tempAQU=data.frame(tempAQU)
latq1 = data.frame(latq1)
latq1 = cbind(tempAQU$tempAQU, latq1)
latq1 = rename(latq1,  Index = `tempAQU$tempAQU` )

#ChiqAQUQ
ChinqGW = q_simn$simulation$ChinqGW
ChinqGW = ChinqGW[1097:2191,]
ChinqGW1 =colMeans(ChinqGW[,2:length(ChinqGW)])
tempAQU=colnames(ChinqGW[,2:length(ChinqGW)])
tempAQU=data.frame(tempAQU)
tempAQU = row.names(tempAQU)
tempAQU=data.frame(tempAQU)
ChinqGW1 = data.frame(ChinqGW1)
ChinqGW1 = cbind(tempAQU$tempAQU, ChinqGW1)
ChinqGW1 = rename(ChinqGW1,  Index = `tempAQU$tempAQU` )

#ChinSURQ
Chinqchan_runoff = q_simn$simulation$Chinqchan_runoff
Chinqchan_runoff = Chinqchan_runoff[1097:2191,]
Chinqchan_runoff1 =colMeans(Chinqchan_runoff[,2:length(Chinqchan_runoff)])
tempAQU=colnames(Chinqchan_runoff[,2:length(Chinqchan_runoff)])
tempAQU=data.frame(tempAQU)
tempAQU = row.names(tempAQU)
tempAQU=data.frame(tempAQU)
Chinqchan_runoff1 = data.frame(Chinqchan_runoff1)
Chinqchan_runoff1 = cbind(tempAQU$tempAQU, Chinqchan_runoff1)
Chinqchan_runoff1 = rename(Chinqchan_runoff1,  Index = `tempAQU$tempAQU` )

#CHINLATQ
Chinqlatq = q_simn$simulation$Chinqlatq
Chinqlatq = Chinqlatq[1097:2191,]
Chinqlatq1 =colMeans(Chinqlatq[,2:length(Chinqlatq)])
tempAQU=colnames(Chinqlatq[,2:length(Chinqlatq)])
tempAQU=data.frame(tempAQU)
tempAQU = row.names(tempAQU)
tempAQU=data.frame(tempAQU)
Chinqlatq1 = data.frame(Chinqlatq1)
Chinqlatq1 = cbind(tempAQU$tempAQU, Chinqlatq1)
Chinqlatq1 = rename(Chinqlatq1,  Index = `tempAQU$tempAQU` )

#BasinPerc
perco = q_simn$simulation$Perc
perco = perco[1097:2191,]
perco1 =colMeans(perco[,2:length(perco)])
tempAQU=colnames(perco[,2:length(perco)])
tempAQU=data.frame(tempAQU)
tempAQU = row.names(tempAQU)
tempAQU=data.frame(tempAQU)
perco1 = data.frame(perco1)
perco1 = cbind(tempAQU$tempAQU, perco1)
perco1 = rename(perco1,  Index = `tempAQU$tempAQU` )

#ChinPerc
chinPerc = q_simn$simulation$ChinqPerc
chinPerc = chinPerc[1097:2191,]
chinPerc1 =colMeans(chinPerc[,2:length(chinPerc)])
tempAQU=colnames(chinPerc[,2:length(chinPerc)])
tempAQU=data.frame(tempAQU)
tempAQU = row.names(tempAQU)
tempAQU=data.frame(tempAQU)
chinPerc1 = data.frame(chinPerc1)
chinPerc1 = cbind(tempAQU$tempAQU, chinPerc1)
chinPerc1 = rename(chinPerc1,  Index = `tempAQU$tempAQU` )



outputs = cbind(AQUNO3toREACH1, AQU_FLOW2Chan1$AQU_FLOW2Chan1, 
                CropYield1$CropYield1, runoffNO3LOSS1$runoffNO3LOSS1, 
                Fert1$Fert1, chan_runoff1$chan_runoff1, latq1$latq1,
                ChinqGW1$ChinqGW1, Chinqchan_runoff1$Chinqchan_runoff1, Chinqlatq1$Chinqlatq1,
                perco1$perco1, chinPerc1$chinPerc1)

outputs$TotFlow = outputs[,3] + outputs[,7] + outputs[,8] #Channel Flows 3 = aquQ, 8 = surQ, 9 = latq
outputs$AQUFracBasin = outputs[,3]/outputs$TotFlow
outputs$SurFracBasin = (outputs[,7] + outputs[,8])/outputs$TotFlow

#outputs$TotFlowChinq = outputs[,11] + outputs[,12] + outputs[,13] #Chinq Channel Flows 11 = aquQ, 12 = surQ, 13 = latq
#outputs$AQUFracChinq = outputs[,11]/outputs$TotFlowChinq
#outputs$SurFracChinq = (outputs[,12] + outputs[,13])/outputs$TotFlowChinq



no3_sim = q_simn$simulation$no3
#no3_sim = no3_sim[1097:2191,]

flow_sim = q_simn$simulation$flow
#flow_sim = flow_sim[1097:2191,]



#Fit correct dates from simulation to observed file
no3_sim<-dplyr::filter(no3_sim,date %in% no3_obs$date)
flow_sim<-dplyr::filter(flow_sim,date %in% no3_obs$date)


flow_simID=row.names(flow_sim)
flow_simID = data.frame(flow_simID)
flow_sim = cbind(flow_simID, flow_sim)
flow_simID = cbind(flow_simID, flow_sim$date)

flow_sim = flow_sim[,3:length(flow_sim)]
row_sub = apply(flow_sim, 1, function(row) all(row !=0 ))
##Subset as usual
flow_sim = flow_sim[row_sub,]

flow_sim$flow_simID=row.names(flow_sim)

flowdate<-dplyr::filter(flow_simID,flow_simID %in% flow_sim$flow_simID)


flow_sim = cbind(flowdate[,2], flow_sim)   

flow_sim = rename(flow_sim,  date = `flowdate[, 2]` )


flow_sim$flow_simID = NULL

no3_sim<-dplyr::filter(no3_sim,date %in% flow_sim$date)
#######################

#convert flow from m^3/sec to L/sec, fit frames, filter flow >0, convert to mg/L
flowgood = flow_sim[,2:length(flow_sim)]
summary(flowgood[,1])
flowgood = flowgood * 1000
summary(flowgood[,1])


###Investigating NO3out
kg_day = no3_sim
kg_day_1 = kg_day[,2:length(kg_day)]
mg_day = kg_day_1*1000000
mg_sec = mg_day/86400
mg_sec = cbind(kg_day[1], mg_sec)


mg_sec = mg_sec[,2:length(mg_sec)]


mg_L = mg_sec/flowgood

flowgood= flowgood/1000 
flowgood = cbind(flow_sim[,1], flowgood)
mg_L = cbind(flow_sim[,1], mg_L)

####Calculate Nutrient NSE scores_ generate plot and param info


no3_obs=na.omit(no3_obs)

mg_L = rename(mg_L,  date = `flow_sim[, 1]` )


f<-dplyr::filter(mg_L,date %in% no3_obs$date)
no3_obs<-dplyr::filter(no3_obs,date %in% mg_L$date)






###Calculate NSE Scores
#prep NSE obs and sim matricies
run_sel_sub = f[, c(2:ncol(f))]
no3matrix=run_sel_sub
no3matrix[, c(1:ncol(no3matrix))] = no3_obs$no3

#Get NSE scores, filter out threshold data
run_sel = data.frame(NSE(sim =run_sel_sub, obs = no3matrix))
run_sel$simID=row.names(run_sel)
temp=colnames(run_sel)
run_sel = rename(run_sel,  NSE = temp[1] )
#run_sel
min(run_sel$NSE)
max(run_sel$NSE)
###SET NSE THRESHOLD
threshold= min(run_sel$NSE)
run_sel = run_sel[run_sel$NSE > threshold,]
#run_sel
#select simulations based on run_sel$simID
no3_plus1 =f %>% select(run_sel$simID)
no3_plus1= cbind(f[, 1], no3_plus1)
#par_set


temp2 = run_sel %>% separate(simID, c("run", "num"))
temp2$num = as.numeric(temp2$num)
run_sel = cbind(run_sel, temp2$num)
run_sel = rename(run_sel,  par_setID = "temp2$num" )
par_set_vis=par_set
par_set_vis = par_set_vis[c(run_sel$par_setID), ]
par_set_vis$NSE = run_sel$NSE
par_set_visNO3=par_set_vis
#write.csv(par_set_vis, "par_set_vis_NO3.csv")


####
sens = lm(NSE ~ .,data=par_set_vis)
#summary(sens)

no3_plus1 = rename(no3_plus1, date = "f[, 1]" )

####
no3_plot <- no3_obs %>% 
  #  rename(q_obs = discharge) %>% # Rename the discharge columnt to q_obs
  #  filter(year(date) %in% 2005:2018) %>% # Filter for years between 2003 and 2012
  left_join(., no3_plus1, by = 'date') %>% # Join with the q_plus table by date
  #left_join(., q_2012, by = 'date') %>% # Join with the q_plus table by date
  pivot_longer(., cols = -date, names_to = 'variable', values_to = 'no3') # Make a long table for plotting

#Create jpeg name and size
jpeg(file="CapeFear90m_NO3.jpeg", width = 1600, height = 800)

#set line type to solid
lineset =colnames(no3_plus1)
lineset = replace(lineset, 1:length(lineset), "solid")
plot_colors = lineset
plot_colors = replace(plot_colors, 2:length(plot_colors), "red")
plot_colors = replace(plot_colors, 1, "black")


ggplot(data = no3_plot) +
  geom_line(aes(x = date, y = no3, col = variable, lty = variable)) +
  scale_color_manual(values = plot_colors) +
  scale_linetype_manual(values = lineset) + 
  theme(legend.position="none")

dev.off()

####Flow Plotting with NO3 overlap


q_obs=read.csv("ChinquapinUSGS_05_18_dailyavg.csv")

q_obs$X=NULL
#q_obs

q_obs = q_obs %>% separate(date, c("year", "month","day"))

q_obs$date2 = paste(q_obs$year, q_obs$month, q_obs$day, sep = "-")
q_obs$date3<-as.Date(q_obs$date2,format='%Y-%m-%d')

q_obs$month = NULL
q_obs$day= NULL
q_obs$year= NULL
q_obs$date2= NULL
q_obs$date= q_obs$date3
q_obs$date3= NULL
q_obs$d2=q_obs$discharge
q_obs$discharge=NULL
q_obs$discharge=q_obs$d2
q_obs$d2 = NULL

#q_obs<-dplyr::filter(q_obs,date %in% no3_obs$date)


f = q_simn$simulation$flow
#Fit correct dates from simulation to observed file
f<-dplyr::filter(f,date %in% q_obs$date)

###Calculate NSE Scores
#prep NSE obs and sim matricies
run_sel_sub = f[, c(2:ncol(f))]
qmatrix=run_sel_sub
qmatrix[, c(1:ncol(qmatrix))] = q_obs$discharge

#Get NSE scores, filter out threshold data
run_sel = data.frame(NSE(sim =run_sel_sub, obs = qmatrix))
run_sel$simID=row.names(run_sel)
temp=colnames(run_sel)
run_sel = rename(run_sel,  NSE = temp[1] )
#run_sel
min(run_sel$NSE)
max(run_sel$NSE)
###SET NSE THRESHOLD
threshold= min(run_sel$NSE)
run_sel = run_sel[run_sel$NSE > threshold,]
#run_sel
#select simulations based on run_sel$simID
q_plus1 =f %>% select(run_sel$simID)
q_plus1= cbind(f[, 1], q_plus1)
#par_set


temp2 = run_sel %>% separate(simID, c("run", "num"))
temp2$num = as.numeric(temp2$num)
run_sel = cbind(run_sel, temp2$num)
run_sel = rename(run_sel,  par_setID = "temp2$num" )
par_set_vis=par_set
par_set_vis = par_set_vis[c(run_sel$par_setID), ]
par_set_vis$NSE = run_sel$NSE
#par_set_vis
#write.csv(par_set_vis, "par_set_vis_FLOW_Outlet44.csv")

par_set_vis$Index = row.names(par_set_vis)
par_set_visNO3$Index = row.names(par_set_visNO3)

l = length(par_set_visNO3)-1
l1 = length(par_set_visNO3)
par_set_visNO3 = par_set_visNO3[ ,c(l,l1)]

#par_set_visFULL= join(par_set_vis, par_set_visNO3, by = 'Index', type = "inner", match = "all")
#par_set_visFULL=inner_join(par_set_vis, par_set_visNO3, by ='Index')

par_set_visFULL<-dplyr::filter(par_set_vis,Index %in% par_set_visNO3$Index)
par_set_visNO3<-dplyr::filter(par_set_visNO3,Index %in% par_set_visFULL$Index)



par_set_visFULL = cbind(par_set_visFULL, par_set_visNO3$NSE)
##Add all SWAT PLUS outputs to parset output file

par_set_visFULL=inner_join(par_set_visFULL, outputs, by ='Index')




write.csv(par_set_visFULL, "par_set_vis_FULL_Outlet44.csv")

no3_sim = q_simn$simulation$no3
no3_sim = no3_sim[1097:2191,]
no3_sim = no3_sim[,2:length(no3_sim)] * 1000000/86400
  
flow_sim = q_simn$simulation$flow
flow_sim = flow_sim[1097:2191,]
flowsimL = flow_sim[,2:length(flow_sim)] *1000

no3_sim1 = no3_sim/flowsimL
no3_sim1 = cbind(flow_sim[,1], no3_sim1)
write.csv(no3_sim1, "NO3SimMatrix.csv")

write.csv(flow_sim, "FlowSimMatrix.csv")

####
sens = lm(NSE ~ .,data=par_set_vis)
#summary(sens)


####
q_plot <- q_obs %>% 
  rename(q_obs = discharge) %>% # Rename the discharge columnt to q_obs
  filter(year(date) %in% 2005:2007) %>% # Filter for years between 2003 and 2012
  left_join(., q_plus1, by = 'date') %>% # Join with the q_plus table by date
  #left_join(., q_2012, by = 'date') %>% # Join with the q_plus table by date
  pivot_longer(., cols = -date, names_to = 'variable', values_to = 'discharge') # Make a long table for plotting

#Create jpeg name and size
jpeg(file="CapeFear90m_flow.jpeg", width = 1600, height = 800)

#set line type to solid
lineset =colnames(q_plus1)
lineset = replace(lineset, 1:length(lineset), "solid")
plot_colors = lineset
plot_colors = replace(plot_colors, 2:length(plot_colors), "red")
plot_colors = replace(plot_colors, 1, "black")


ggplot(data = q_plot) +
  geom_line(aes(x = date, y = discharge, col = variable, lty = variable)) +
  scale_color_manual(values = plot_colors) +
  scale_linetype_manual(values = lineset) + 
  theme(legend.position="none")

dev.off()


###
#max(run_sel$NSE) # look up value, enter run below in mg_L
#reg=lm(no3_obs$no3~ mg_L$run_318)
#plot(no3_obs$no3, mg_L$run_318)
#abline(reg)

# take out bad value
#reg=lm(no3_obs$no3[c(1:12,14)]~ mg_L$run_318[c(1:12,14)])
#plot(no3_obs$no3[c(1:12,14)], mg_L$run_318[c(1:12,14)])
#abline(reg)

#reg=lm(no3_obs$no3[c(1:12)]~ mg_L$run_318[c(1:12)])
#plot(no3_obs$no3[c(1:12)], mg_L$run_318[c(1:12)])
#abline(reg)
