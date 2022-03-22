# import libraries
install.packages("corrplot")

library(gridExtra)
library(smooth)
library(fpp2)
library(corrplot)

# read data
avocado = read.csv('avocado.csv')

####################################################################
################### Part 1: describe the data ######################
####################################################################

summary(avocado)
names(avocado)


# Plot Average Price.
ggplot(avocado) +
  geom_boxplot(aes(x=region, y=AveragePrice, color = region), alpha=0.3) +
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Avocado Average Price") +
  theme(legend.position="none") 

avocado_detroit_org = avocado[avocado$region == "Detroit" & avocado$type == "organic",]

####################################################################
################### Part 2: decompose ##############################
####################################################################

tsavocado = ts(avocado_detroit_org, freq=365.25/7)
tsavocado
autoplot(tsavocado[, 'AveragePrice'])
autoplot(tsavocado[, 'Total.Volume'])
add.decomp = decompose(tsavocado[, 'Total.Volume'], "additive")
autoplot(add.decomp)

avocado_detroit_org['Total.Volume'] = tsclean(avocado_detroit_org$Total.Volume)
tsavocado = ts(avocado_detroit_org, freq=365.25/7)

####################################################################
################### Part 3: Models #################################
####################################################################

train_data = window(tsavocado, end = 3.6-1/(365.25/7))
test_data  = window(tsavocado, start = 3.6)

nrow(train_data)
nrow(test_data)

Volume = train_data[,"Total.Volume"]

volume.naive = naive(Volume, h=length(test_data[,"Total.Volume"]))
volume.ave   = meanf(Volume, h=length(test_data[,"Total.Volume"])) 
volume.drift = rwf(Volume, drift=TRUE, h=length(test_data[,"Total.Volume"]))
volume.sma   = sma(Volume, h=length(test_data[,"Total.Volume"]))
volume.ses   = ses(Volume, h=length(test_data[,"Total.Volume"]))
volume.ets   = forecast(Volume, h=length(test_data[,"Total.Volume"]))

fit.arma = auto.arima(Volume, d=0, seasonal=FALSE)
volume.arma  = forecast(fit.arma, h=length(test_data[,"Total.Volume"]))

fit.arima = auto.arima(Volume, seasonal=FALSE)
volume.arima  = forecast(fit.arima, h=length(test_data[,"Total.Volume"]))

fit.sarima = auto.arima(Volume)
volume.sarima = forecast(fit.sarima, h=length(test_data[,"Total.Volume"])) 

autoplot(tsavocado[,"Total.Volume"]) +
  autolayer(volume.naive$mean, series="Naive") +
  autolayer(volume.ave$mean, series="Average") +
  autolayer(volume.drift$mean, series="Drift") +
  autolayer(volume.sma$forecast, series="SMA") +
  autolayer(volume.ses$mean, series="SES") +
  autolayer(volume.ets$mean, series="ETS") +
  autolayer(volume.arma$mean, series="ARMA") +
  autolayer(volume.arima$mean, series="ARIMA") +
  autolayer(volume.sarima$mean, series="SARIMA")

# Zoom in
autoplot(window(tsavocado[,"Total.Volume"], start = 3.6)) +
  autolayer(volume.naive$mean, series="Naive") +
  autolayer(volume.ave$mean, series="Average") +
  autolayer(volume.drift$mean, series="Drift") +
  autolayer(volume.sma$forecast, series="SMA") +
  autolayer(volume.ses$mean, series="SES") +
  autolayer(volume.ets$mean, series="ETS") +
  autolayer(volume.arma$mean, series="ARMA") +
  autolayer(volume.arima$mean, series="ARIMA") +
  autolayer(volume.sarima$mean, series="SARIMA")


accuracy(volume.naive$mean, test_data[,"Total.Volume"])
accuracy(volume.ave$mean, test_data[,"Total.Volume"])
accuracy(volume.drift$mean, test_data[,"Total.Volume"])
accuracy(volume.sma$forecast, test_data[,"Total.Volume"])
accuracy(volume.ses$mean, test_data[,"Total.Volume"])
accuracy(volume.ets$mean, test_data[,"Total.Volume"])
accuracy(volume.arma$mean, test_data[,"Total.Volume"])
accuracy(volume.arima$mean, test_data[,"Total.Volume"])
accuracy(volume.sarima$mean, test_data[,"Total.Volume"])


####################################################################
################ Part 4: Dynamic linear regression #################
####################################################################

corrMatrix = as.matrix(subset(avocado_detroit_org, select=c('AveragePrice','Total.Volume','X4046','X4225','X4770','Total.Bags','Small.Bags','Large.Bags')))
corrplot(cor(corrMatrix), method="circle")

options(warn=-1)

Price = train_data[,"AveragePrice"]
SmallBag = train_data[,"Small.Bags"]
LargeBag = train_data[,"Large.Bags"]

extvar = cbind(Price, SmallBag, LargeBag)
extvar_test = cbind(test_data[,"AveragePrice"], test_data[,"Small.Bags"], test_data[,"Large.Bags"])

dlr.fit = auto.arima(Volume, xreg = extvar)
summary(dlr.fit)
dlr.fc = forecast(dlr.fit, xreg = extvar_test)
accuracy(dlr.fc$mean, test_data[,"Total.Volume"])
checkresiduals(dlr.fit)

df = data.frame(Volume, Price, SmallBag, LargeBag)
lm.fit = lm(Volume ~ Price + SmallBag + LargeBag, data = df)
summary(lm.fit)
testData = data.frame(extvar_test)
lm.fc = predict(lm.fit, newdata = data.frame(Price=testData[,1], SmallBag=testData[,2], LargeBag=testData[,3]))
accuracy(lm.fc, test_data[,"Total.Volume"])
checkresiduals(lm.fit$residuals)
dlr.fc$mean
lm.fc

autoplot(window(tsavocado[,"Total.Volume"], start = 3.6)) +
  autolayer(volume.ets$mean, series="ETS") +
  autolayer(volume.sarima$mean, series="SARIMA") +
  autolayer(dlr.fc$mean, series="DLR") 


####################################################################
############### Part 5: Dynamic harmonic regression ################
####################################################################

fit  = list()
fc   = list()
aicc = list()
p    = list()

for (i in seq(6)) { 
  fit[[i]] = auto.arima(Volume, xreg = fourier(Volume, K=i), seasonal=FALSE)
  fc[[i]] = forecast(fit[[i]], xreg = fourier(Volume, K=i, h = length(test_data[,"Total.Volume"])))
  aicc[[i]] = fit[[i]]$aicc
  
  p[[i]] = autoplot(fc[[i]]) +
    ggtitle(paste("K =",i))  +
    ylab("")
}

aicc

autoplot(ts(aicc)) # minimum is k = 5

grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], nrow = 3, ncol = 2)

fit[[5]]

# Since using K = 1 gives us the smallest AICc, we use this as our final model.
accuracy(fc[[5]]$mean, test_data[,"Total.Volume"])

autoplot(window(tsavocado[,"Total.Volume"], start = 3.6)) +
  autolayer(volume.sarima$mean, series="SARIMA") +
  autolayer(volume.arima$mean, series="ARIMA") + 
  autolayer(dlr.fc$mean, series="DLR") + 
  autolayer(fc[[5]]$mean, series="Fourier") 


####################################################################
#################### Part 6: Neural Network ########################
####################################################################

fit_nn = nnetar(Volume)
fc.nn = forecast(fit_nn, h = length(test_data[,"Total.Volume"]))
accuracy(fc.nn$mean, test_data[,"Total.Volume"])

autoplot(window(tsavocado[,"Total.Volume"], start = 3.6)) +
  autolayer(volume.sarima$mean, series="SARIMA") +
  autolayer(volume.arima$mean, series="ARIMA") +
  autolayer(fc.nn$mean, series = "NN") + 
  autolayer(fc[[5]]$mean, series="Fourier")

####################################################################
################ Part 7: Forecast combination ######################
####################################################################

# Forecast combination using averaging
fc.comb1 = (volume.arima$mean + volume.ets$mean + fc.nn$mean + fc[[5]]$mean + dlr.fc$mean)/5
accuracy(fc.comb1, test_data[,"Total.Volume"])

fc.comb2 = (volume.ets$mean + fc.nn$mean + fc[[5]]$mean)/3
accuracy(fc.comb2, test_data[,"Total.Volume"])

####################################################################
###### Part 8: More sophisticated forecast combinations ############
####################################################################

install.packages("opera")
library(opera)

X = cbind(ETS = volume.ets$mean, NN = fc.nn$mean, DHR = fc[[5]]$mean)

mixt1 = mixture(model = "MLpol", loss.type = "square")
mixt2 = mixture(model = "OGD", loss.type = "square")

weights1 = predict(mixt1, X, test_data[,"Total.Volume"], type="weights")
weights2 = predict(mixt2, X, test_data[,"Total.Volume"], type="weights")

response1 = predict(mixt1, X, test_data[,"Total.Volume"], type="response")
response2 = predict(mixt2, X, test_data[,"Total.Volume"], type="response")

mixt1
mixt2

# the last row is the optimal weight
tail(weights1)
tail(weights2)

fc.comb3 = 0*volume.ets$mean + 1*fc.nn$mean + 0*fc[[5]]$mean
fc.comb4 = 0.3075391*volume.ets$mean + 0.5098171*fc.nn$mean + 0.1826438*fc[[5]]$mean

accuracy(volume.naive$mean, test_data[,"Total.Volume"]) 
accuracy(volume.ave$mean, test_data[,"Total.Volume"])
accuracy(volume.drift$mean, test_data[,"Total.Volume"])
accuracy(volume.sma$forecast, test_data[,"Total.Volume"])
accuracy(volume.ses$mean, test_data[,"Total.Volume"]) 
accuracy(volume.ets$mean, test_data[,"Total.Volume"]) 
accuracy(volume.arma$mean, test_data[,"Total.Volume"])
accuracy(volume.arima$mean, test_data[,"Total.Volume"]) 
accuracy(volume.sarima$mean, test_data[,"Total.Volume"]) 
accuracy(dlr.fc$mean, test_data[,"Total.Volume"]) 
accuracy(lm.fc, test_data[,"Total.Volume"]) 
accuracy(fc[[5]]$mean, test_data[,"Total.Volume"]) 
accuracy(fc.nn$mean, test_data[,"Total.Volume"]) 

accuracy(fc.comb1, test_data[,"Total.Volume"]) 
accuracy(fc.comb2, test_data[,"Total.Volume"]) 
accuracy(as.numeric(response1), test_data[,"Total.Volume"]) 
accuracy(as.numeric(response2), test_data[,"Total.Volume"]) 
accuracy(fc.comb3, test_data[,"Total.Volume"]) 
accuracy(fc.comb4, test_data[,"Total.Volume"])


autoplot(window(tsavocado[,"Total.Volume"], start = 3.6)) +
  autolayer(fc.comb1, series="COMB1") + 
  autolayer(fc.comb2, series="COMB2") + 
  autolayer(fc.comb3, series="COMB3") +
  autolayer(fc.comb4, series="COMB4")

####################################################################
################## Part 9: forecast for future #####################
####################################################################

fc_Volume = tsavocado[,"Total.Volume"]

fc_volume.ets = forecast(fc_Volume, h=4)
fc_fit_nn = nnetar(fc_Volume)
fc_nn = forecast(fc_fit_nn, h = 4)

fc.fit.arima = auto.arima(fc_Volume, seasonal=FALSE)
fc.volume.arima  = forecast(fc.fit.arima, h=4)

fc_fit5 = auto.arima(fc_Volume, xreg = fourier(fc_Volume, K=5), seasonal=FALSE)
fc_fc5  = forecast(fc_fit5, xreg = fourier(fc_Volume, K=5, h = 4))

fc.Price = tsavocado[,"AveragePrice"]
fc.SmallBag = tsavocado[,"Small.Bags"]
fc.LargeBag = tsavocado[,"Large.Bags"]
fc.extvar = cbind(fc.Price, fc.SmallBag, fc.LargeBag)
fc.dlr.fit = auto.arima(fc_Volume, xreg = fc.extvar)
fc.dlr = forecast(fc.dlr.fit, xreg = fc.extvar)

fc_avg_comb =(fc.volume.arima$mean + fc_volume.ets$mean + fc_nn$mean + fc_fc5$mean + fc.dlr$mean)/5

fc_avg_comb

autoplot(tsavocado[,"Total.Volume"]) +
  autolayer(fc_avg_comb, series = "Average Comb") 





