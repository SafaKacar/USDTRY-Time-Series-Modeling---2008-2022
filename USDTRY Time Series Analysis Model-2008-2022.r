install.packages("TSA")     #Install packages- TSA#
library("TSA")  
install.packages("caschrono")
install.packages("forecast")
library("caschrono")
install.packages("fUnitRoots")
library("fUnitRoots")
library("forecast")
install.packages("pdR")
library(pdR)


win.graph(width=4.875, height=2.5,pointsize=8)
PROJECTts=ts(PROJECT)
plot(PROJECT$`PPI 08-22`,ylab='Producer Price Index',xlab='Date Between  2008 January- 2022 December',type='o')
acf(PROJECT$`PPI 08-22`)
pacf(PROJECT$`PPI 08-22`)
plot(bc,ylab='Producer Price Index',xlab='Date Between  2008 January- 2022 December',type='o')
acf(bc)
pacf(bc)
lambda <- BoxCox.lambda(PROJECT$`PPI 08-22`)
lambda#lambda is 0.7850252, It is not close to 1 so we need to Box-Cox Transformation
bc=BoxCox(PROJECT$`PPI 08-22`,lambda)
lambdabc <- BoxCox.lambda(bc)
lambdabc#lambda is 0.9266912, It is close to 1.
bc=BoxCox(PROJECT$`PPI 08-22`,lambda)
plot(BoxCox(PROJECT$`PPI 08-22`,lambda))

ac=BoxCox.ar(PROJECT$`PPI 08-22`,method = c("ols"),type="o")


#philip-perron unit root test
pp.test(bc)#reject Ho, series is stationary

#Augmented Dickey Fuller (ADF) unit root test
adfTest(bc, type="c")#reject Ho, series is stationary
adfTest(bc, type="ct")#reject Ho, series is deterministic

#kpss test
kpss.test(bc,null=c("Level"))#do not reject Ho, series is stationary
kpss.test(bc,null=c("Trend"))#do not reject Ho, series is deterministic and no need to take difference

#kpss.test(diff(PROJECT$PPI.08.16),null=c("Level"))
#kpss.test(diff(PROJECT$PPI.08.16),null=c("Trend"))
#model2=lm(PROJECT$PPI.08.16~time(PROJECT$PPI.08.16))
summary(PROJECT$PPI.08.16)

eacf(bc)
armaselect(bc)
armaselect(PROJECT$`PPI 08-22`)

outsp=HEGY.test(wts=bc,regvar=0, selectlags=list(mode="aic", Pmax=8))
outsp
