# MAE Econometrics Group project 1;
rm(list=ls(all=TRUE))
#Making a Standardization function;
std.iz<-function(x){
  s<- (x-mean(x))/mean(x)
  return(s)
}
x<-c(1:10)
std.iz(x)
#Question2;

library(quantmod)
sp500 <- new.env()
getSymbols("^GSPC", env = sp500, src = "yahoo",from=as.Date("2000-10-01"),to=as.Date("2014-10-01"))
GSPC <- sp500$GSPC
head(GSPC)
(GSPC) #Make Sure is Daily;
sp.prc<-GSPC$GSPC.Adjusted
return.sp<-dailyReturn(GSPC) #calculating Daily Retuns
head(return.sp)
chartSeries(GSPC)
#Plot Histogram and Density Curve
truehist(return.sp, col="skyblue3",ylab="Counts",xlab="Values",main="Daily Returns on S&P500")
lines(density(return.sp),lwd=2,col="red3")
legend(1,0.4,  c("Histogram", "Density"),fill=c("skyblue3","red3"),cex=1)

#summary statistics;
names(GSPC)
sp.stock<-GSPC$GSPC.Adjusted
summary(sp.stock)
sd(sp.stock)
retrn.sp<-dailyReturn(sp.stock)




#Amazon Example;
amnz.env<-new.env()
getSymbols("AMZN", env = amnz.env, src = "yahoo")
amzn <- amnz.env$AMZN
head(amzn)
amzn.prc<-amzn$AMZN.Adjusted
periodicity(amzn) #Make Sure is Daily;
return.amzn<-dailyReturn(amzn.prc) #calculating Daily Retuns
head(return.amzn)
chartSeries(amzn)
#Plot Histogram and Density Curve
truehist(return.amzn, col="skyblue3",ylab="Counts",xlab="Values",main="Daily Returns on Amazon")
lines(density(return.amzn),lwd=2,col="red3")
legend(1,0.4,  c("Histogram", "Density"),fill=c("skyblue3","red3"),cex=1)
cor(return.sp,return.amzn)

#APPLE
apl.env<-new.env()
getSymbols("AAPL", env = apl.env, src = "yahoo")
names(apl.env)
apl <- apl.env$AAPL
head(apl)
apl.prc<-apl$AAPL.Adjusted
return.apl<-dailyReturn(apl.prc) #calculating Daily Retuns
head(return.apl)
chartSeries(apl)
#Plot Histogram and Density Curve
truehist(return.apl, col="skyblue3",ylab="Counts",xlab="Values",main="Daily Returns on Apple")
lines(density(return.apl),lwd=2,col="red3")
legend(1,0.4,  c("Histogram", "Density"),fill=c("skyblue3","red3"),cex=1)
cor(return.sp,return.apl)

#GOOGLE
goog.env<-new.env()
getSymbols("GOOG", env = goog.env, src = "yahoo")
names(goog.env)
goog<- goog.env$GOOG
head(goog)
goog.prc<-goog$GOOG.Adjusted
return.goog<-dailyReturn(goog.prc) #calculating Daily Retuns
head(return.goog)
chartSeries(goog)
#Plot Histogram and Density Curve
truehist(return.goog, col="skyblue3",ylab="Counts",xlab="Values",main="Daily Returns on Google")
lines(density(return.goog),lwd=2,col="red3")
legend(1,0.4,  c("Histogram", "Density"),fill=c("skyblue3","red3"),cex=1)
cor(return.sp,return.goog)



#NETFLIX
nfx.env<-new.env()
getSymbols("NFLX", env = nfx.env, src = "yahoo")
names(nfx.env)
nfx<- nfx.env$NFLX
head(nfx)
nfx.prc<-nfx$NFLX.Adjusted
return.nfx<-dailyReturn(nfx.prc) #calculating Daily Retuns
head(return.nfx)
chartSeries(nfx)
#Plot Histogram and Density Curve
truehist(return.nfx, col="skyblue3",ylab="Counts",xlab="Values",main="Daily Returns on Netflix")
lines(density(return.nfx),lwd=2,col="red3")
legend(1,0.4,  c("Histogram", "Density"),fill=c("skyblue3","red3"),cex=1)
cor(return.sp,return.nfx)

nfx.exc<-c(return.nfx-return.sp)
head(nfx.exc)
sum(nfx.exc>0.03)
# Total number of time difference > 3%


###Creating Box Plots for all 5 Stock Indicies***************************************;
install.packages("ggplot2")
library(ggplot2)
prc.data<-data.frame(cbind(sp.prc, amzn.prc,apl.prc,goog.prc,nfx.prc))
boxplot(std.iz(prc.data))
boxplot(prc.data$GOOG.Adjusted)

#Question 3;

prior=c(C=0.01, not.C = 1-C ) 
like.D = c(pos=.99, neg=.01)
like.not.D = c(pos=.01, neg=.99)
likelihood = rbind(D = like.D, not.D = like.not.D)
likelihood

for i in (1:n)
while pr < 0.95

