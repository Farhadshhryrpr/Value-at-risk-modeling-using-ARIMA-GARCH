library(fGarch)
library(rugarch)
library(pdR)
library(dplyr)
library(tidyr)
library(readxl)
library(forecast)
library(fractal)
library(devtools)
library(fracdiff)
library(ggplot2)
library(tseries)
library(aTSA)
library(urca)
library(splus2R)
library(ifultools)
library(moments)
library(PerformanceAnalytics)
library(rugarch)
library(stats)
library(devtools)
library(afmtools)
library(xts)












setwd("C:/Users/HP/Desktop/Project/1")
d <- read_excel("d.xlsx", col_types = c("numeric","numeric",
                                        "text", "text",
                                        "text"))
df<-unite(d,Date , y, m,d ,sep ="-")

df$Date<-as.Date(df$Date, "%Y-%m-%d")
#########################################################

##ggplot-index since 4-12-1397

#########################################################
ggplot(df,aes(x=Date)) +
  geom_line(aes(y=index ),alpha=0.9  , size=0.7)+
  theme(panel.background = element_rect(fill = "white", colour = "black") , panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.text.x =element_text(size=14,angle = 45, vjust = 1, hjust=1),axis.text.y =element_text(size=17),axis.title =element_blank())+
  scale_x_date(date_breaks ="6 months" ,
               date_labels = "%Y-%m")+labs(title = "Tehran Index")
##########################################################

##Density plot and qq plot of index

##########################################################
ts.index = ts(df$index)

#Histogram
hist(ts.index, xlab="Tehran Index", prob=TRUE,
     main="Histogram of Tehran Index")
xfit<-seq(min(ts.index),max(ts.index),length=40)
yfit<-dnorm(xfit,mean=mean(ts.index),sd=sd(ts.index))
lines(xfit, yfit, col="blue", lwd=2)

##qqplot
chart.QQPlot(ts.index, distribution = "t", df=10)
chart.QQPlot(ts.index, distribution = "std")
chart.QQPlot(ts.index, distribution = "sstd")
chart.QQPlot(ts.index, distribution = "norm")

##########################################################
plot(density(ts.index))
##########################################################

## d parameter

#########################################################
fdGPH(ts.index)##1.15
##So we get diff of log return
lr=diff(ts.index)/ts.index[-length(ts.index)]
fdGPH(lr)##0.45
chart.QQPlot(lr, distribution = "norm")
a= auto.arima(lr)
a
##then
ddlr=diff(lr)
fdGPH(ddlr)##-0.48
#########################################################

plot(ddlr, main = "Stationary Time series of Tehran INDEX",
     ylab = "Difference of Log Return",
     xlab ="Time")

##qqplot
chart.QQPlot(ddlr, distribution = "t", df=5)
chart.QQPlot(ddlr, distribution = "std")
chart.QQPlot(ddlr, distribution = "sstd")
chart.QQPlot(ddlr, distribution = "norm")
#########################################################
plot(density(ddlr))

#Histogram
hist(ddlr, xlab="Tehran Index", prob=TRUE,
     main="Histogram of Difference of Log Return of Tehran INDEX")
xfit<-seq(min(ddlr),max(ddlr),length=40)
yfit<-dnorm(xfit,mean=mean(ddlr),sd=sd(ddlr))
lines(xfit, yfit, col="blue", lwd=2)
#########################################################
##normality test
jarque.bera.test(ddlr)#0.28
##noise?
obs = length(ddlr)
Box.test(ddlr, lag=log(obs), c("Box-Pierce"))#1.013e-13
Box.test(ddlr, lag=log(obs), c("Ljung-Box"))#7.649e-14
########################################################
basicStats(ddlr)
########################################################

##BEST ARIMA FIT

#######################################################
a = auto.arima(ddlr)#ARIMA(1,0,2)
a
#######################################################
data = ddlr
p=8
q=8
aic.y <- array(0, dim=c(p,q)) 
bic.y <- array(0, dim=c(p,q)) 
hqc.y <- array(0, dim=c(p,q)) 

for (i in 1:p)
  for (k in 1:q){ 
    arimafit <- arima(data, order=c(i-1,0,k-1),
                      method = c("ML"),
                      optim.control = list(maxit=10000), 
                      optim.method = "BFGS",
                      kappa = 1e6, xreg = NULL,
                      include.mean = FALSE)
    if(i>=k) {
      n = obs - i 
    } else { 
      n = obs - k 
    }
    aic.y[i,k] <- AIC(arimafit, k=2)
    bic.y[i,k] <- AIC(arimafit, k=log(n))
    hqc.y[i,k] <- AIC(arimafit, k=2*log(log(n)))
  }

aic.min <- min(aic.y)
bic.min <- min(bic.y)
hqc.min <- min(hqc.y)

for (ii in 1:p)
  for (kk in 1:q){
    if (aic.y[ii,kk]==aic.min)
    {
      p.aic.y <- ii-1
      q.aic.y <- kk-1
    }
  }

for (ii in 1:p)
  for (kk in 1:q){
    if (bic.y[ii,kk]==bic.min)
    {
      p.bic.y <- ii-1
      q.bic.y <- kk-1
    }
  }

for (ii in 1:p)
  for (kk in 1:q){
    if (hqc.y[ii,kk]==hqc.min)
    {
      p.hqc.y <- ii-1
      q.hqc.y <- kk-1
    }
  }

paste("AIC:","p=",p.aic.y, "q=",q.aic.y)#arima(6,7)
paste("BIC:","p=",p.bic.y, "q=",q.bic.y)#arima(0,2)
paste("HQC:","p=",p.hqc.y, "q=",q.hqc.y)#arima(0,2)
###################################################
##Test the result of auto arima
res12 = residuals(a)
acf(res12,main="Residuals of ARIMA(1, 2) ", lag.max = 40)
pacf(res12,main="Residuals of ARIMA(1, 2) ", lag.max = 40)
###################################################
##Test the result of AIC###BEST Result
a67 =arima(ddlr,c(6,0,7),
           optim.control = list(maxit=100000),
           kappa = 1e6 , method = 'ML')
a67
res67 = residuals(a67)
acf(res67,main="Residuals of ARIMA(6, 7) ", na.action = na.pass, lag.max = 40)
pacf(res67,main="Residuals of ARIMA(6, 7) ", na.action = na.pass, lag.max = 40)
#####
#Tests
####
##normality test
jarque.bera.test(res67)#0.24
##noise?
Box.test(res67, lag=log(obs), c("Box-Pierce"))#0.99
Box.test(res67, lag=log(obs), c("Ljung-Box"))#0.99
####################################################
##result of HQC & BS
a02 =arima(ddlr,c(0,0,2),
           optim.control = list(maxit=100000),
           kappa = 1e6 , method = 'CSS-ML')
a02
res02 = residuals(a02)
acf(res02,main="Residuals of ARIMA(0, 2)", na.action = na.pass, lag.max = 40)
pacf(res02,main="Residuals of ARIMA(0, 2)", na.action = na.pass, lag.max = 40)
#####################################################
##ARCH EFFECT
acf(res67^2,main="squared Residuals of ARIMA(6, 7) ", na.action = na.pass, lag.max = 40)
pacf(res67^2,main="Squared Residuals of ARIMA(6, 7) ", na.action = na.pass, lag.max = 40)

Box.test(res67^2, lag=log(obs), c("Box-Pierce"))#2.596e-13
Box.test(res67^2, lag=log(obs), c("Ljung-Box"))#1.709e-13
#####################################################

##GARCH MODELING

####################################################





