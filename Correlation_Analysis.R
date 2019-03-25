#Read data
Station_S1_ANM = cbind(StationSum_ANM[,2:5],MEISum[,2:5])
Station_S2_ANM = cbind(StationSum_ANM[,2:5],DMISum[,2:5])
Station_S3_ANM = cbind(StationSum_ANM[,2:5],NINO34Sum[,2:5])
Station_S4_ANM = cbind(StationSum_ANM[,2:5],NINO3Sum[,2:5])
Station_S5_ANM = cbind(StationSum_ANM[,2:5],NINO4Sum[,2:5])

#Correlation with MEI
for(i in 1:4)
{
  Y = Station_S1_ANM[, i]
  X = Station_S1_ANM[,i+4]
  CorMEI = cor(Y,X)
  CorMEI = cor.test(Y,X)
  print(CorMEI)
}

## Correlation with DMI
for(i in 1:4)
{
  X = Station_S2_ANM[,i+4]
  Y = Station_S2_ANM[, i]
  CorDMI = cor(Y,X)
  CorDMI = cor.test(Y,X)
  print(CorDMI)
}

#Correlation with NINO34
for(i in 1:4)
{
  Y = Station_S3_ANM[, i]
  X = Station_S3_ANM[,i+4]
  CorNINO34 = cor(Y,X)
  CorNINO34 = cor.test(Y,X)
  print(CorNINO34)
}

#Correlation with NINO3
for(i in 1:4)
{
  Y = Station_S4_ANM[, i]
  X = Station_S4_ANM[,i+4]
  CorNINO3 = cor(Y,X)
  CorNINO3 = cor.test(Y,X)
  print(CorNINO3)
}

#Correlation with NINO4
for(i in 1:4)
{
  Y = Station_S1_ANM[, i]
  X = Station_S_ANM[,i+4]
  CorNINO4 = cor(Y,X)
  CorNINO4 = cor.test(Y,X)
  print(CorNINO4)
}

