
StationSum_C1_ANM =cbind(StationSum_ANM_cat,MEISum[,2:5],DMISum[,2:5]) #three classes
StationSum_C2_ANM =cbind(StationSum_Sqrt_ANM_cat,MEISum[,2:5],DMISum[,2:5]) #three classes
StationSum_C3_ANM =cbind(StationSum_log_ANM_cat,MEISum[,2:5],DMISum[,2:5]) #three classes
StationSum_C4_ANM =cbind(StationSum_Sqd_ANM_cat,MEISum[,2:5],DMISum[,2:5]) #three classes
StationSum_C5_ANM =cbind(StationSum_ANM_cat2,MEISum[,2:5],DMISum[,2:5]) #two classes
StationSum_C6_ANM =cbind(StationSum_Sqrt_ANM_cat2,MEISum[,2:5],DMISum[,2:5]) #two classes
StationSum_C7_ANM =cbind(StationSum_log_ANM_cat2,MEISum[,2:5],DMISum[,2:5]) #two classes
StationSum_C8_ANM =cbind(StationSum_Sqd_ANM_cat2,MEISum[,2:5],DMISum[,2:5]) #two classes 

#Change the name of dataframe to Station to use above dataframes for the classification
Station = StationSum_C1_ANM 


for(i in 4:4)
{
  Y <- Station[,i]
  X1 <- Station[,4+i]
  X2 <- Station[,8+i]
  
  Tree_Station = tree(Y~X1+X2, data = Station, na.action = na.pass, method = "recursive.partition")  ## fit tree model
  print(summary(Tree_Station))
  pruned.tree_Station <- prune.tree(Tree_Station, best=6)
  pruned.tree1<-prune.misclass(Tree_Station, best=6)
  PTree_predict_Station = predict(pruned.tree_Station, data=Station) ## predict with tree model training data
  PTree_Train_Result_Station = 1-(misclass.tree(pruned.tree_Station,detail = FALSE)/nrow(Station))
  E = misclass.tree(pruned.tree_Station, detail = FALSE)
  
  z=c("dry"="red","avg"="blue","wet"="black")
  plot(X1,X2,  pch=19, cex=1,col=z, ylim = c(-0.8,1.0), xlim=c(-2.1,2.4))
  partition.tree(pruned.tree_Station, label = "Rainfall", add=T, ordvars=c("X1","X2") , ylim = c(-0.8,1.0), xlim=c(-2.1,2.4))
  partition.tree(Tree_Station, label = "Rainfall", add=T, ordvars=c("X1","X2") , ylim = c(-0.8,1.0), xlim=c(-2.1,2.4))
  
  Test_In = as.character(Station[,i]) ## actual y
  Test_Out <- colnames(PTree_predict_Station)[max.col(PTree_predict_Station, ties.method = c("random"))] # predicted
  Tree_Test_Result = 1-(mean (Test_In != Test_Out)) # misclassification %
  table(Test_Out,Station[,i] )
  
  plot(pruned.tree_Station)
  plot(Tree_Station)
  count = table(Station[,i])
  print(count)
}
