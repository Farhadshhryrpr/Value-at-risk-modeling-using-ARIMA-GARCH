

data =ddlr
var = c()
quantile = 0.05
num = 0
start = 230
n = length(data) - start
for(i in 1:n){
  training = data[i:(start+i-1)]
  arma.aparch.t = ugarchspec(variance.model = list(garchOrder=c(1,1), model="gjrGARCH"),
                             mean.model = list(armaOrder=c(6,7), include.mean = TRUE,
                                               archm = FALSE, archpow = 1, arfima =FALSE,
                                               external.regressors = NULL, archex =FALSE),
                             distribution.model = "norm" ) 
  print(i)
  training = data[i:(start+i-1)]
  fit1 = ugarchfit(data = training, spec=arma.aparch.t)
  if(is.null(fit1@fit$residuals)){
    var = c(var, 0)
    next
  }
  
  pre = ugarchforecast(fit1,n.ahead=1,data=training)
  mu = as.numeric(pre@forecast$seriesFor[1])
  sd = as.numeric(pre@forecast$sigmaFor[1])
  tem = -mu - sd * qnorm(quantile, 0, 1)
  var = c(var, tem)
}
test = data[(start + 1): length(data)]
correct = 0
total = 0
print(var)
print(test)
print(length(test))
for(i in 1:length(test)){
  if (var[i] == "NaN") {
    next
  }
  if (var[i] == "Inf") {
    next
  }
  if(var[i] == 0){
    next
  }
  total = total + 1
  if(test[i] <= -var[i]){
    correct = correct + 1
  }
}
print(total)
hit = correct/total
hit
var = c(var, correct)
var = c(var, total)
var

####
fit1
garchvol <- sigma(fit1)##fit1 must be for GJR-GARCH(1,1)

length(garchVaR)
##########################################################
garchroll <- ugarchroll(arima.gjrGarch, data = ddlr, n.start = 350,
                        refit.window = "moving", refit.every = 100)
garchVaR <- quantile(garchroll, probs = 0.05)
actual <- xts(as.data.frame(garchroll)$Realized, time(garchVaR))
VaRplot(alpha = 0.05, actual = actual, VaR = garchVaR)

##########################################################
ggplot(data = garchVaR, aes(x=df$Date[351:459])) +
  geom_line(aes(y= garchVaR ),alpha=0.9  , size=0.7)+
  theme(panel.background = element_rect(fill = "white", colour = "black") , panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.text.x =element_text(size=14,angle = 45, vjust = 1, hjust=1),axis.text.y =element_text(size=17),axis.title =element_blank())+
  scale_x_date(date_breaks ="1 months" ,
               date_labels = "%Y-%m")+
  labs(title = "Predicted VaR by GJR-GARCH(1, 1)+ARMA(6,7)")
##########################################################
preds <- as.data.frame(garchroll)
preds
garchvol <- xts(preds$Sigma, order.by = as.Date(rownames(preds)))
ggplot(data = garchvol, aes(x=df$Date[351:459])) +
  geom_line(aes(y= garchvol ),alpha=0.9  , size=0.7)+
  theme(panel.background = element_rect(fill = "white", colour = "black") , panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.text.x =element_text(size=14,angle = 45, vjust = 1, hjust=1),axis.text.y =element_text(size=17),axis.title =element_blank())+
  scale_x_date(date_breaks ="1 months" ,
               date_labels = "%Y-%m")+
  labs(title = "Predicted Volatility by GJR-GARCH(1, 1)+ARMA(6,7)")
