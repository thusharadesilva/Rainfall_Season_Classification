# Data normality test and data transformed to normal distribution

##Plot data distribution (corresponding to the plots of the manuscript)
A=StationSum[,4]
A1=StationSum_Sqrt_ANM_SD[,2] 
A2=StationSum_ANM_SD[,2] 
xy = density(A1)
plot(xy, main =" " , xlab="X_(S_ANM)")
abline(v=+0.5, col=2)
abline(v=-0.5,col=2 )


A = StationSum[,2]
A1= StationSum_ANM_SD[,2] 
B= StationSum_Sqrt[,2]
B1= StationSum_Sqrt_ANM_SD[,2]
dev.off()
par(mfrow=c(2,2),oma = c(3,2,0,0) + 0.1,mar = c(1,1,1,0)  )
qqnorm(A1,main = " ");qqline(A1, col = 2)
qqnorm(B1,main = " " );qqline(B1, col = 2)
xy = density(A1); plot(xy, main =" " , xlab="X_(S_ANM)")
xy = density(B1); plot(xy, main =" " , xlab="X_(S_ANM)")


attach(Station)
x1<- StationSum_ANM$V4
StationSum = as.data.frame(StationSum)
x3=StationSum$NEM
x2<-log(x3)

qqnorm(x1);qqline(x1, col = 2)
qqnorm(x2);qqline(x2, col = 2)
plot(density(x1));plot(density(x2))
shapiro.test(x1); shapiro.test(x2)

for (i in 2:2) 
{ x1<- StationSum_ANM[,i]
  x2<-log(StationSum[,i])
  x3<-log10(StationSum[,i])
  x4<-(StationSum[,i])*(StationSum[,i])
  x5<-sqrt(StationSum[,i])
  x6<-exp(StationSum[,i])
  x7<-exp(StationSum_ANM[,i])
  shapiro.test(x1)
  shapiro.test(x2)
  shapiro.test(x3)
  shapiro.test(x4)
  shapiro.test(x5)
  shapiro.test(x6)
  shapiro.test(x7)
}

qqnorm(x1,main = " ");qqline(x1, col = 2)
qqnorm(x2,main = " ");qqline(x2, col = 2)
qqnorm(x4,main = " ");qqline(x4, col = 2)
qqnorm(x5,main = " ");qqline(x5, col = 2)
