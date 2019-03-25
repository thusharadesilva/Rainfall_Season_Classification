StationSum_C1_ANM =cbind(StationSum_ANM_cat,MEISum[,2:5],DMISum[,2:5]) #three classes
StationSum_C2_ANM =cbind(StationSum_Sqrt_ANM_cat,MEISum[,2:5],DMISum[,2:5]) #three classes
StationSum_C3_ANM =cbind(StationSum_log_ANM_cat,MEISum[,2:5],DMISum[,2:5]) #three classes
StationSum_C4_ANM =cbind(StationSum_Sqd_ANM_cat,MEISum[,2:5],DMISum[,2:5]) #three classes
StationSum_C5_ANM =cbind(StationSum_ANM_cat2,MEISum[,2:5],DMISum[,2:5]) #two classes
StationSum_C6_ANM =cbind(StationSum_Sqrt_ANM_cat2,MEISum[,2:5],DMISum[,2:5]) #two classes
StationSum_C7_ANM =cbind(StationSum_log_ANM_cat2,MEISum[,2:5],DMISum[,2:5]) #two classes
StationSum_C8_ANM =cbind(StationSum_Sqd_ANM_cat2,MEISum[,2:5],DMISum[,2:5]) #two classes 

#Change the name as Station for use above dataframes to classification
Station = StationSum_C1_ANM 

### Seasonal Rainfall data classified
index <- createDataPartition(StationSum_C5_ANM[,2], p=0.75, list=FALSE)
trainSet_Station <- StationSum_C5_ANM[ index,]
testSet_Station <- StationSum_C5_ANM[-index,]
Test_accuracy = matrix(data =NA, nrow=1, ncol=4)
Check_Error = matrix(data =NA, nrow=nrow(testSet_Station), ncol=4)
A = matrix(data =NA, nrow=1, ncol=4)



for (i in 2:5)
{
  Y <- trainSet_Station[,i]
  X1 <- trainSet_Station[,4+i]
  X2 <- trainSet_Station[,8+i]
  testSet_Station1 = testSet_Station
  
  fit<- randomForest(Y ~ X1+X2, trainSet_Station,ntree=500)
  summary(fit)
  ##Predict Output 
  testSet_Station1 <- data.frame(X1 = testSet_Station1[,i+4], X2 = testSet_Station1[, i+8])
  
  predicted= predict(fit,newdata=testSet_Station1)
  Check_Error[,i-1] = (predicted == testSet_Station[,i])
  A = sum(Check_Error[,i-1], na.rm=TRUE)
  Test_accuracy[,i-1] = A/nrow(testSet_Station)
  print(A)
  print(fit)
  plot(fit)
  
  reprtree:::plot.getTree(fit)
  table(predicted,testSet_Station[,i] )
  importance(fit)
}

