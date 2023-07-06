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
path = 'E:/Cf_project_90m/Swat projects/BermAgCornWheatSplits/BermAgCornWheatSplits/Scenarios/TarheelGWcalhalfLifeUpNO3MaxAquNO3100/TxtInOut/'


n <- 10
#locked perco, try surlag.bsn too
par_set <- tibble(
  'cn2.hru | change = abschg' = c(-10,-5),
  'cn3_swf.hru | change = abschg' = c(.1,1),
 # 'chn.rte | change = abschg' = c(.05,.25),
#  'wd_rto.rte | change = absval' = c(.5,20),
#  'lat_len.hru | change = absval' = c(22, 40),
  'canmx.hru | change = abschg' = c(-4,.5),
  'latq_co.hru | change = pctchg' = c(-50,-10),
  'perco.hru | change = pctchg' = c(-5,9),
  'epco.hru | change = pctchg' = c(-25,0),
  'esco.hru | change = pctchg' = c(-50,0),
  'k.sol | change = pctchg' = c(-50,50),
  'awc.sol | change = pctchg' = c(30,60),
    'ovn.hru | change = pctchg' = c(8,10),
  'flo_min.aqu | change = absval' = c(5,8),
  'alpha.aqu | change = absval' = c(.3,.5),
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
  'revap_co.aqu | change = absval' = c(0.15,.25),
  'deep_seep.aqu | change = absval' = c(0.05,0.25),
  'nperco.bsn | change = absval' = c(0.8,1),
  'cdn.bsn | change = absval' = c(0,.2),
  'sdnco.bsn | change = absval' = c(1,1.5),
  'n_updis.bsn | change = absval' = c(.01,.1),
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
Y[,18] <- qunif(Y[,18], min(par_set[18]), max(par_set[18]))
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
                                                          unit = 13),
                                     no3 = define_output(file = "channel_sd",
                                                         variable = "no3_out",
                                                         unit = 13),
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
                                     precip = define_output(file = "basin_wb",
                                                          variable = "precip", #basin wide perc into vadose zone (mm H2O)
                                                          unit = 1),
                                     FloodplainStorage = define_output(file = "aquifer",    #Basin-wide aqu flow into channels (mm H2O)
                                                        variable = "stor",
                                                        unit = 31),
                                     UplandStorage = define_output(file = "aquifer",    #Basin-wide aqu flow into channels (mm H2O)
                                                               variable = "stor",
                                                               unit = 28),
                                     FloodplainNO3 = define_output(file = "aquifer",    #Basin-wide aqu flow into channels (mm H2O)
                                                             variable = "no3_st",
                                                             unit = 31),
                                     UplandStorageNO3 = define_output(file = "aquifer",    #Basin-wide aqu flow into channels (mm H2O)
                                                             variable = "no3_st",
                                                             unit = 28)
                                     ),
                       
                       parameter = par_set,
                       n_thread = 10)

#FloodPlain NO3 GW
FloodplainNO3 = q_simn$simulation$FloodplainNO3
FloodplainStorage = q_simn$simulation$FloodplainStorage
FloodplainStorage_1 = FloodplainStorage
FloodplainNO3_1 = FloodplainNO3
FloodplainNO3_1 = FloodplainNO3_1[,2:ncol(FloodplainNO3_1)]
FloodplainStorage_1 = FloodplainStorage_1[,2:ncol(FloodplainStorage_1)]

FloodplainStorage_1 = FloodplainStorage_1/100
FloodplainNO3_1 = FloodplainNO3_1/FloodplainStorage_1
FloodplainNO3_1 = cbind(FloodplainNO3[,1], FloodplainNO3_1)

#Upland NO3 GW
UplandStorageNO3 = q_simn$simulation$UplandStorageNO3
UplandStorage = q_simn$simulation$UplandStorage
UplandStorage_1 = UplandStorage
UplandStorageNO3_1 = UplandStorageNO3
UplandStorageNO3_1 = UplandStorageNO3_1[,2:ncol(UplandStorageNO3_1)]
UplandStorage_1 = UplandStorage_1[,2:ncol(UplandStorage_1)]

UplandStorage_1 = UplandStorage_1/100
UplandStorageNO3_1 = UplandStorageNO3_1/UplandStorage_1
UplandStorageNO3_1 = cbind(UplandStorageNO3[,1], UplandStorageNO3_1)

write.csv(FloodplainNO3_1, "CF_tarheel_FloodplainAQU_NO3.csv")
write.csv(UplandStorageNO3_1, "CF_tarheel_UplandAQU_NO3.csv")


uplandOBSNO3 = read.csv("UplandAQUTarheelCF_GWNO3_Obs.csv")
FloodPlainOBSNO3 = read.csv("CapeFearChinquapinfloodplainOBSNO3.csv")

uplandOBSNO3 = uplandOBSNO3 %>% separate(sample_dat, c("month", "day","year"))
uplandOBSNO3$date2 = paste(uplandOBSNO3$year, uplandOBSNO3$month, uplandOBSNO3$day, sep = "-")
uplandOBSNO3$date3<-as.Date(uplandOBSNO3$date2,format='%Y-%m-%d')

uplandOBSNO3_1 = uplandOBSNO3
uplandOBSNO3_1$date = uplandOBSNO3$date3
uplandOBSNO3_1$NO3=uplandOBSNO3$nitrate
uplandOBSNO3_1 = uplandOBSNO3_1[,c(10:11)]

FloodPlainOBSNO3 = FloodPlainOBSNO3 %>% separate(sample_dat, c("month", "day","year"))
FloodPlainOBSNO3$date2 = paste(FloodPlainOBSNO3$year, FloodPlainOBSNO3$month, FloodPlainOBSNO3$day, sep = "-")
FloodPlainOBSNO3$date3<-as.Date(FloodPlainOBSNO3$date2,format='%Y-%m-%d')

FloodPlainOBSNO3_1 = FloodPlainOBSNO3
FloodPlainOBSNO3_1$date = FloodPlainOBSNO3$date3
FloodPlainOBSNO3_1$NO3=FloodPlainOBSNO3$nitrate
FloodPlainOBSNO3_1 = FloodPlainOBSNO3_1[,c(10:11)]







#Fit correct dates from simulation to observed file
UplandStorageNO3_1<-dplyr::filter(UplandStorageNO3_1,date %in% uplandOBSNO3_1$date)
FloodplainNO3_1<-dplyr::filter(FloodplainNO3_1,date %in% FloodPlainOBSNO3_1$date)
#Go back and add notes fitting the OBS to the sim data and everything should line up

uplandOBSNO3_1<-dplyr::filter(uplandOBSNO3_1,date %in% UplandStorageNO3_1$date)

#####

q_obs=read.csv("CFTarheel_02_18Dailyavg.csv")
q_obs$X=NULL
q_obs
#q_obs = q_obs %>% separate(RDate, c("month", "day","year"))
#q_obs$date2 = paste(q_obs$year, q_obs$month, q_obs$day, sep = "-")
q_obs$date3<-as.Date(q_obs$RDate,format='%Y-%m-%d')
#q_obs$month = NULL
#q_obs$day= NULL
#q_obs$year= NULL
#q_obs$date2= NULL
q_obs$date= q_obs$date3
q_obs$date3= NULL
#q_obs$d2=q_obs$discharge
#q_obs$discharge=NULL
q_obs$discharge=q_obs$avgWL
q_obs$avgWL = NULL
q_obs$RDate = NULL



######
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
run_sel
min(run_sel$NSE)
max(run_sel$NSE)
###SET NSE THRESHOLD
threshold= min(run_sel$NSE)
run_sel = run_sel[run_sel$NSE > threshold,]
run_sel
#select simulations based on run_sel$simID
q_plus1 =f %>% select(run_sel$simID)
q_plus1= cbind(f[, 1], q_plus1)
par_set


temp2 = run_sel %>% separate(simID, c("run", "num"))
temp2$num = as.numeric(temp2$num)
run_sel = cbind(run_sel, temp2$num)
run_sel = rename(run_sel,  par_setID = "temp2$num" )
par_set_vis=par_set
par_set_vis = par_set_vis[c(run_sel$par_setID), ]
par_set_vis$NSE = run_sel$NSE
par_set_vis
write.csv(par_set_vis, "par_set_vis_CFTarheel.csv")

write.csv(UplandStorageNO3_1, "filteredAQUNO3.csv")


no3_plot <- uplandOBSNO3_1 %>% 
  #  rename(q_obs = discharge) %>% # Rename the discharge columnt to q_obs
  #  filter(year(date) %in% 2005:2018) %>% # Filter for years between 2003 and 2012
  left_join(., UplandStorageNO3_1, by = 'date') %>% # Join with the q_plus table by date
  #left_join(., q_2012, by = 'date') %>% # Join with the q_plus table by date
  pivot_longer(., cols = -date, names_to = 'variable', values_to = 'no3') # Make a long table for plotting

#Create jpeg name and size
jpeg(file="CapeFear90m_NO3.jpeg", width = 1600, height = 800)

#set line type to solid
lineset =colnames(UplandStorageNO3_1)
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

pp = colSums(UplandStorageNO3_1[,2:length(UplandStorageNO3_1)])
write.csv(pp, "pp.csv")

#prep full range Surface water and GW NO3 records
write.csv(f, "TarheelSurfaceFlowOut.csv")
