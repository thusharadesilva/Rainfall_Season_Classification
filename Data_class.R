rm(list = ls())

library(randomForest)
library(caret)
library(rlang)
library(Matrix)
library(tree)
library(openxlsx)
library(tibble)
library(reprtree)
library(MASS)
library(klaR)

# Read data
Station = read.csv("Station.csv", header = TRUE)
MEI = read.csv("MEI1.csv", header = TRUE)
DMI = read.csv("DMI1.csv" , header = TRUE)
NINO34 = read.csv("NINO3.4.csv",header = TRUE)
NINO3 = read.csv("NINO3.csv", header = TRUE)
NINO4 = read.csv("NINO4.csv", header = TRUE)  

#Creat blank matrics for data store
MEISum = matrix(data = NA, nrow = 64, ncol = 5)
DMISum = matrix(data = NA, nrow = 64, ncol = 5)
NINO34Sum = matrix(data = NA, nrow = 64, ncol = 5)
NINO3Sum = matrix(data = NA, nrow = 64, ncol = 5)
NINO4Sum = matrix(data = NA, nrow = 64, ncol = 5)
StationSum = matrix(data = NA, nrow = 64, ncol = 5)
StationSum_Sqrt = matrix(data = NA, nrow = 64, ncol = 5)
StationSum_log = matrix(data = NA, nrow = 64, ncol = 5)
StationSum_Sqd = matrix(data = NA, nrow = 64, ncol = 5)
StationSum_ANM = matrix(data = NA, nrow = 64, ncol = 5)
StationSum_ANM_SD = matrix(data = NA, nrow = 64, ncol = 5)
StationSum_ANM_cat = matrix(data = NA, nrow = 64, ncol = 5) # for rainfall 3 classes
StationSum_Sqrt_ANM = matrix(data = NA, nrow = 64, ncol = 5)
StationSum_Sqrt_ANM_SD = matrix(data = NA, nrow = 64, ncol = 5)
StationSum_Sqrt_ANM_cat = matrix(data = NA, nrow = 64, ncol = 5) # for rainfall 3 classes
StationSum_Sqrt_ANM_cat2 = matrix(data = NA, nrow = 64, ncol = 5) # for rainfall 3 classes
StationSum_log_ANM = matrix(data = NA, nrow = 64, ncol = 5)
StationSum_log_ANM_SD = matrix(data = NA, nrow = 64, ncol = 5)
StationSum_log_ANM_cat = matrix(data = NA, nrow = 64, ncol = 5) # for rainfall 3 classes
StationSum_log_ANM_cat2 = matrix(data = NA, nrow = 64, ncol = 5) # for rainfall 3 classes
StationSum_Sqd_ANM = matrix(data = NA, nrow = 64, ncol = 5)
StationSum_Sqd_ANM_SD = matrix(data = NA, nrow = 64, ncol = 5)
StationSum_Sqd_ANM_cat = matrix(data = NA, nrow = 64, ncol = 5) # for rainfall 3 classes
StationSum_Sqd_ANM_cat2 = matrix(data = NA, nrow = 64, ncol = 5) # for rainfall 3 classes
StationSum_ANM_cat2 = matrix(data = NA, nrow = 64, ncol = 5) # for rainfall 3 classes
StationSum_ANM_cat3 = matrix(data = NA, nrow = 64, ncol = 5) # for rainfall 3 classes
StationSum_ANM = as.data.frame(StationSum_ANM)
StationSum_ANM_cat = as.data.frame(StationSum_ANM_cat)
StationSum_ANM_cat2 = as.data.frame(StationSum_ANM_cat2)
StationSum_ANM_cat3 = as.data.frame(StationSum_ANM_cat3)
StationSum_Sqrt_ANM_cat = as.data.frame(StationSum_Sqrt_ANM_cat)
StationSum_Sqrt_ANM = as.data.frame(StationSum_Sqrt_ANM)
StationSum_Sqrt_ANM_SD = as.data.frame(StationSum_Sqrt_ANM_SD)
StationSum_Sqrt = as.data.frame(StationSum_Sqrt)
StationSum_Sqrt_ANM_cat2 = as.data.frame(StationSum_Sqrt_ANM_cat)
StationSum_log_ANM_cat = as.data.frame(StationSum_log_ANM_cat)
StationSum_log_ANM = as.data.frame(StationSum_log_ANM)
StationSum_log_ANM_SD = as.data.frame(StationSum_log_ANM_SD)
StationSum_log_ANM_cat2 = as.data.frame(StationSum_log_ANM_cat)
StationSum_Sqd_ANM_cat = as.data.frame(StationSum_Sqd_ANM_cat)
StationSum_Sqd_ANM = as.data.frame(StationSum_Sqd_ANM)
StationSum_Sqd_ANM_SD = as.data.frame(StationSum_Sqd_ANM_SD)
StationSum_Sqd_ANM_cat2 = as.data.frame(StationSum_Sqd_ANM_cat)

#Divide data into rainfall seasons
#Areal rainfall data of Station
StationSum[,1] = Station$Year
StationSum[,2] = Station$Jan+Station$Feb
StationSum[,3] = Station$Apr+Station$Mar
StationSum[,4] = Station$May+Station$Jun+Station$Jul+Station$Aug+Station$Sep
StationSum[,5] = Station$Oct+Station$Nov+Station$Dec

# MEI (to represent the ENSO)

MEISum [1:64] = Station$Year[1:64]
MEISum[,2] = ave(MEI$DECJAN, MEI$JANFEB, MEI$FEBMAR)
MEISum[,3] = ave(MEI$MARAPR,MEI$APRMAY)
MEISum[,4] = ave(MEI$MAYJUN,MEI$JUNJUL, MEI$JULAUG, MEI$AUGSEP)
MEISum[,5] = ave(MEI$SEPOCT, MEI$OCTNOV)

# DMI (to represent the IOD)
DMISum [1:64] = Station$Year[1:64]
DMISum[,2] = ave(DMI$DECJAN, DMI$JANFEB, DMI$FEBMAR)
DMISum[,3] = ave(DMI$MARAPR,DMI$APRMAY)
DMISum[,4] = ave( DMI$MAYJUN,DMI$JUNJUL, DMI$JULAUG, DMI$AUGSEP)
DMISum[,5] = ave(DMI$SEPOCT, DMI$OCTNOV)

##NINO34  (to represent the ENSO)
NINO34Sum [1:64] = Station$Year[1:64]
NINO34Sum[,2] = ave(NINO34$Jan, NINO34$Feb, NINO34$Mar)
NINO34Sum[,3] = ave(NINO34$Apr,NINO34$May, NINO34$Jun)
NINO34Sum[,4] = ave(NINO34$Jul, NINO34$Aug, NINO34$Sep)
NINO34Sum[,5] = ave(NINO34$Oct, NINO34$Nov, NINO34$Dec)

##NINO3  (to represent the ENSO)
NINO3Sum [1:64] = Station$Year[1:64]
NINO3Sum[,2] = ave(NINO3$Jan, NINO3$Feb, NINO3$Mar)
NINO3Sum[,3] = ave(NINO3$Apr,NINO3$May, NINO3$Jun)
NINO3Sum[,4] = ave(NINO3$Jul, NINO3$Aug, NINO3$Sep)
NINO3Sum[,5] = ave(NINO3$Oct, NINO3$Nov, NINO3$Dec)

# NINO4  (to represent the ENSO)
NINO4Sum1 [1:64] = Station$Year[1:64]
NINO4Sum1[,2] = ave(NINO4$Jan, NINO4$Feb, NINO4$Dec)
NINO4Sum1[,3] = ave(NINO4$Apr,NINO4$Mar)
NINO4Sum1[,4] = ave(NINO4$May,NINO4$Jun,NINO41$Jul, NINO4$Aug, NINO4$Sep)
NINO4Sum1[,5] = ave(NINO4$Oct, NINO4$Nov)

colnames(StationSum)  <- c("Year", "NEM", "FIM", "SWM", "SIM")
colnames(MEISum)  <- c("Year", "M_NEM", "M_FIM", "M_SWM", "M_SIM")
colnames(DMISum)  <- c("Year", "D_NEM", "D_FIM", "D_SWM", "D_SIM")

#Rainfall anomaly calculation
for(i in 2:5)
{
  StationSum_ANM[,i] = StationSum[,i] - mean(StationSum[,i])
  StationSum_ANM_SD[,i] = (StationSum[,i] - mean(StationSum[,i]))/sd(StationSum[,i])
  StationSum_ANM_cat[,i]= cut(StationSum_ANM_SD[,i],c(-10,-0.5,0.5,10),labels=c('dry','avg','wet'))
  StationSum_ANM_cat2[,i]= cut(StationSum_ANM_SD[,i],c(-10,-0.5,10),labels=c('dry','notdry'))
  StationSum_Sqrt[,i] = sqrt(StationSum[,i])
  StationSum_Sqrt_ANM[,i] = StationSum_Sqrt[,i] - mean(StationSum_Sqrt[,i])
  StationSum_Sqrt_ANM_SD[,i] = (StationSum_Sqrt[,i] - mean(StationSum_Sqrt[,i]))/sd(StationSum_Sqrt[,i])
  StationSum_Sqrt_ANM_cat[,i]= cut(StationSum_Sqrt_ANM_SD[,i],c(-10,-0.5,0.5,10),labels=c('dry','avg','wet')) 
  StationSum_Sqrt_ANM_cat2[,i]= cut(StationSum_Sqrt_ANM_SD[,i],c(-10,-0.5,10),labels=c('dry','notdry')) 
  StationSum_log[,i] = log(StationSum[,i])
  StationSum_log_ANM[,i] = StationSum_log[,i] - mean(StationSum_log[,i])
  StationSum_log_ANM_SD[,i] = (StationSum_log[,i] - mean(StationSum_log[,i]))/sd(StationSum_log[,i])
  StationSum_log_ANM_cat[,i]= cut(StationSum_log_ANM_SD[,i],c(-10,-0.5,0.5,10),labels=c('dry','avg','wet')) 
  StationSum_log_ANM_cat2[,i]= cut(StationSum_log_ANM_SD[,i],c(-10,-0.5,10),labels=c('dry','notdry')) 
  StationSum_Sqd[,i] = (StationSum[,i])*(StationSum[,i])
  StationSum_Sqd_ANM[,i] = StationSum_Sqd[,i] - mean(StationSum_Sqd[,i])
  StationSum_Sqd_ANM_SD[,i] = (StationSum_Sqd[,i] - mean(StationSum_Sqd[,i]))/sd(StationSum_Sqd[,i])
  StationSum_Sqd_ANM_cat[,i]= cut(StationSum_Sqd_ANM_SD[,i],c(-10,-0.5,0.5,10),labels=c('dry','avg','wet')) 
  StationSum_Sqd_ANM_cat2[,i]= cut(StationSum_Sqd_ANM_SD[,i],c(-10,-0.5,10),labels=c('dry','notdry')) 
}



