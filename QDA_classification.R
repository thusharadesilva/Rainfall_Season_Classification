#Read data
StationSum_C1_ANM =cbind(StationSum_ANM_cat,MEISum[,2:5],DMISum[,2:5]) #three classes
StationSum_C2_ANM =cbind(StationSum_Sqrt_ANM_cat,MEISum[,2:5],DMISum[,2:5]) #three classes
StationSum_C3_ANM =cbind(StationSum_log_ANM_cat,MEISum[,2:5],DMISum[,2:5]) #three classes
StationSum_C4_ANM =cbind(StationSum_Sqd_ANM_cat,MEISum[,2:5],DMISum[,2:5]) #three classes
StationSum_C5_ANM =cbind(StationSum_ANM_cat2,MEISum[,2:5],DMISum[,2:5]) #two classes
StationSum_C6_ANM =cbind(StationSum_Sqrt_ANM_cat2,MEISum[,2:5],DMISum[,2:5]) #two classes
StationSum_C7_ANM =cbind(StationSum_log_ANM_cat2,MEISum[,2:5],DMISum[,2:5]) #two classes
StationSum_C8_ANM =cbind(StationSum_Sqd_ANM_cat2,MEISum[,2:5],DMISum[,2:5]) #two classes 

#Change the data as Station to select the above data frames

Station = StationSum_C1_ANM 

for(i in 2:5)
{
  Y <- Station[,i]
  X1 <- Station[,4+i]
  X2 <- Station[,8+i]
  
  qda_Station <- qda(Y~X1+X2, data = Station )
  qda_predict_Station = predict(qda_Station, data=Station[,i])
  table(qda_predict_Station$class,Station[,i])
  
  Result_Station = mean(qda_predict_Station$class== Station[,i])
  print(Result_Station)
  name1=c("DMI JFM","MEI JFM")
  partimat(Y~X2+X1,  data=cbind(Station[,i],Station[,i+4],Station[,i+8]), method="qda", name= name1)
  couleurs=c('#66FFFF','#FFFFFF','#FF99FF')
}

