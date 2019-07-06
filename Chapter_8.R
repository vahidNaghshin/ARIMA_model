library(fpp2)
#Prob. 8-2
autoplot(ibmclose)
ggAcf(ibmclose)
ggPacf(ibmclose)
#Prob. 8-3
(lambda <- BoxCox.lambda(usnetelec))
autoplot(BoxCox(usnetelec,lambda))
ggAcf(usnetelec)
ggPacf(usnetelec)
library(urca)
usnetelec %>% ur.kpss() %>% summary()
ndiffs(usnetelec)

usgdp %>% ur.kpss() %>% summary()
ndiffs(usgdp)

mcopper %>% ur.kpss() %>% summary()
ndiffs(mcopper)

enplanements %>% ur.kpss() %>% summary()
ndiffs(enplanements)

visitors %>% ur.kpss() %>% summary()
ndiffs(visitors)

#Problem 8-5
retaildata <- readxl::read_excel("C:/Users/vahid/Documents/retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349873A"],
           frequency=12, start=c(1982,4))
(lambda <- BoxCox.lambda(myts))
autoplot(BoxCox(myts,lambda))
BoxCox(myts,lambda) %>% ur.kpss() %>% summary()
ndiffs(BoxCox(myts,lambda))
BoxCox(myts,lambda) %>% diff() %>% ur.kpss() %>% summary()

#Problem 8-6
# AR(1) model
phi_1 <- 0.6
y <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100)
  y[i] <- phi_1*y[i-1] + e[i]
y %>% autoplot()
#MA(1) model
theta_1 <- 0.6
y <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100) y[i] <- theta_1*e[i-1] + e[i]
y %>% autoplot()

# ARMA model
theta_1 <- 0.6
phi_1 <- 0.6
y <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100) y[i] <- phi_1*y[i-1]+theta_1*e[i-1] + e[i]
y
y %>% autoplot()
y %>% ur.kpss() %>% summary()
ndiffs(y)
#AR(2)
phi_1 <- -0.8
phi_2 <- 0.3
y <- ts(numeric(100))
e <- rnorm(100)
for(i in 3:100)
  y[i] <- phi_2*y[i-2]+phi_1*y[i-1] + e[i]
y %>% autoplot()
ggAcf(y)
ggPacf(y)
Box.test(y, lag=10, type="Ljung-Box")

#Problem 8-7
autoplot(wmurders)
Box.test(wmurders, lag=10, type="Ljung-Box")
wmurder %>% ur.kpss() %>% summary()
ggAcf(wmurders)
ggPacf(wmurders)
(lambda <- BoxCox.lambda(wmurders))
BoxCox(wmurders,lambda) %>% ur.kpss() %>% summary()
ndiffs(BoxCox(wmurders,lambda))
BoxCox(wmurders,lambda) %>% autoplot()
diff(diff(BoxCox(wmurders,lambda))) %>% autoplot()
diff_2 <- diff(diff(BoxCox(wmurders,lambda)))
ggAcf(diff_2)
ggPacf(diff_2)
(fit <- auto.arima(wmurders))
checkresiduals(fit)
fit %>% forecast(h=3) %>% autoplot(include=80)

#Problem 8-8
autoplot(austa)
ggAcf(austa)
ggPacf(austa)
ndiffs(austa)
fit <- auto.arima(austa)
checkresiduals(fit)
fit %>% forecast(h=10) %>% autoplot(include=80)
(fit2 <- Arima(austa, order=c(0,1,1)))
checkresiduals(fit2)
(fit3 <- Arima(austa, order=c(0,1,0)))
checkresiduals(fit3)
fit2 %>% forecast(h=10) %>% autoplot(include=80)
fit3 %>% forecast(h=10) %>% autoplot(include=80)

(fit4 <- Arima(austa, include.mean=T, order=c(2,1,3)))
fit4 %>% forecast(h=10) %>% autoplot(include=80)

(fit5 <- Arima(austa, include.mean=F, order=c(0,0,1)))
fit5 %>% forecast(h=10) %>% autoplot(include=80)

(fit6 <- Arima(austa, include.mean=F, order=c(0,2,1)))
fit6 %>% forecast(h=10) %>% autoplot(include=80)

#Problem 8-9
autoplot(usgdp)
(lambda <- BoxCox.lambda(usgdp))
BoxCox(usgdp,lambda) -> usgdp.Boxcox
autoplot(usgdp.Boxcox)
fit <- auto.arima(usgdp.Boxcox)
checkresiduals(fit)
ggAcf(usgdp.Boxcox)
ggPacf(usgdp.Boxcox)
ndiffs(usgdp.Boxcox)
fit2 <- Arima(usgdp.Boxcox,order=c(1,1,0))
checkresiduals(fit2)
fit %>% forecast(h=10) %>% autoplot(include=80)
fit2 %>% forecast(h=10) %>% autoplot(include=80)
forecast(ets(usgdp), h=10)%>%autoplot()
#Problem 8-10
autoplot(austourists)
(lambda <- BoxCox.lambda(austourists))
BoxCox(austourists,lambda) -> austourists.BoxCox
ggAcf(austourists.BoxCox)
ggPacf(austourists.BoxCox)
austourists.BoxCox %>% diff(lag=4) %>% ggtsdisplay()
fit <- auto.arima(austourists.BoxCox, stepwise=FALSE, approximation=FALSE)
fit1 <- Arima(euretail, order=c(1,0,0), seasonal=c(1,1,1))
#Problem 8-11
autoplot(usmelec)
ets(usmelec) %>% autoplot()
(lambda <- BoxCox.lambda(usmelec))
BoxCox(usmelec,lambda) -> usmelec.BoxCox
ets(usmelec.BoxCox) %>% autoplot()
ndiffs(usmelec.BoxCox)
fit <- auto.arima(usmelec.BoxCox, stepwise=FALSE, approximation=FALSE)
checkresiduals(fit)
fit2 <- Arima(usmelec.BoxCox, order=c(2,1,2), seasonal=c(2,1,1))
fit %>% forecast(h=15) %>% autoplot(include=80)
#Problem 8-16
autoplot(sheep)
sheep %>% diff() %>% ggAcf()
sheep %>% diff() %>% ggPacf()
forecast(auto.arima(sheep), h=4) %>% autoplot()
#Problem 8-18
library(Quandl)
y <- Quandl("EIA/AEO_2016_REF_NO_CPP_PRCE_NA_COMM_NA_NG_NA_SATL_Y13DLRPMCF_A", api_key="YOURE_API_KEY", type="ts")
autoplot(y)
fit <- auto.arima(y)
checkresiduals(fit)
forecast(fit, h=4) %>% autoplot()
ets(y) %>% forecast()%>%autoplot()
ets(y) %>% checkresiduals()