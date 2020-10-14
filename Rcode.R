dt5=data5
summary(dt5)
head(dt5)
colnames(dt5)<-c('date','x')
dt5$date=as.Date(dt5$date,'%Y/%m/%d')
plot(dt5$date,dt5$x,type='b')

x <-diff(dt5$x, lag = 1, differences = 1);plot(dt5$date[1:392],x)
x <-diff(dt5$x, lag = 1, differences = 2);plot(x)
393*0.9
plot(x)
par(mfrow=c(2,1))
acf(x, type = c("correlation"));acf(x, type =c("partial"))
par(mfrow=c(1,1))

dt5=data5
head(dt5)
k=dt5%>%filter(date>='2005-01-01'&date<='2007-12-01')
k=dt5%>%filter(date<='2000-01-01')
plot(k$date,k$x)


spec.pgram(x)  ##  tapr = .1 by default
my.spectrum(phi.of.b=c(1,-my.fit$coef[1:3]), theta.of.b=c(1,my.fit$coef[4]), variance=1)
par(mfrow=c(1,1))
my.fit <- arima(x, order = c(4, 1, 1));my.fit
my.fit <- arima(x, order = c(3, 0, 1));my.fit
acf(my.fit$residuals,type="correlation")
plot(1:my.fit$residuals,main="Residuals")
plot(1:249,my.fit$residuals)

x=data5$Proportion

x=data5$Proportion
my.fit <- arima(x, order = c(3, 2, 1));my.fit
acf(my.fit$residuals,type="correlation")
plot(1:393,my.fit$residuals,main="Residuals",ylab="Residuals")
qqnorm(my.fit$residuals);abline(0,1)

#Forecasting 
x=data5$Proportion
x <-diff(dt5$x, lag = 1, differences = 2);plot(x)
my.fit <- arima(x, order = c(3, 0, 1));my.fit
my.fit <- arima(x, order = c(3, 2, 1));my.fit
my.preds <- predict(my.fit, n.ahead = 9, se.fit = TRUE)
names(my.preds)
my.preds

plot(x,xlim=c(1,410),type="b")

##  Add the predictcions as well as the prediction interval
lines(394:402, my.preds$pred, type="b", col=2)
lines(394:402, my.preds$pred + 2*my.preds$se, type="l", col=3)
lines(394:402, my.preds$pred - 2*my.preds$se, type="l", col=3)
dt5[373,]



plot(373:393,x[373:393],xlim=c(373,403),ylim=c(1.9,2.3),type="b")
lines(394:402, my.preds$pred, type="b", col=2)
lines(394:402, my.preds$pred + 2*my.preds$se, type="l", col=3)
lines(394:402, my.preds$pred - 2*my.preds$se, type="l", col=3)


x=data5$Proportion
x1=x[1:round(0.90*length(x),0)]
my.fit <- arima(x1, order = c(3,3,1))
my.preds <- predict(my.fit, n.ahead = length(x)-length(x1)+9, se.fit = TRUE)
my.preds$pred
a=length(x1)-10
a1=length(x1)
b=length(x)

plot(a:b,x[a:b],xlim=c(a-5,b+10),ylim=c(1.7,2.5),type="b",xlab="time",ylab="data")
lines((a1+1):(b+9), my.preds$pred, type="b", col=2)
lines((a1+1):(b+9), my.preds$pred + 2*my.preds$se, type="l", col=3)
lines((a1+1):(b+9), my.preds$pred - 2*my.preds$se, type="l", col=3)




round(323*0.1,0)

plot(k$x,type='b')
ts.plot(k$x)
my.fit <- arima(k$x, order = c(1, 1, 0))
##  Periodicity?
polyroot(c(1, -my.fit$coef[1])) 
##  The coefficients of the polynomial phi(B)
##  Watch the signs.  What are these the coefficients of?

Mod(polyroot(c(1, -my.fit$coef[1])) )
Arg(polyroot(c(1, -my.fit$coef[1])) )
acf(x, type = c("correlation"), plot=F) 
acf(x, type = c("partial"), plot=F) 
#ar.coef <- c(2.7607, -3.8106, 2.6535, -0.9238)
#my.acf <- ARMAacf(ar = ar.coef, ma = c(0), lag.max = 100, pacf = FALSE)
solve(toeplitz(acf$acf[1]), acf$acf[2])
simdt=arima.sim(n=350,list(ar=c(0.9924)))


par(mfrow=c(2,1))
acf(simdt, type = c("correlation"))
acf(x, type = c("correlation"))
acf(simdt, type = c("partial"))
acf(x, type = c("partial"))
par(mfrow=c(1,1))

ar.coef=c(0.9924)
x=dt5$x
my.fit <- arima(x, order = c(0, 1, 1))
qqnorm(my.fit$residuals);abline(0,1)
x<-arima.sim(model=list(ar=c(my.fit$coef[0]),ma=c(my.fit$coef[1])) , n=350 )

par(mfrow=c(2,1))
acf(x, type = c("correlation"))
acf(x, type = c("partial"))
par(mfrow=c(1,1))

##  Periodicity?
polyroot(c(1, -ar.coef)) 
##  The coefficients of the polynomial phi(B)
##  Watch the signs.  What are these the coefficients of?

Mod(polyroot(c(1, -ar.coef)) )
Arg(polyroot(c(1, -ar.coef)) )

##  and the --TRUE-- periods are (perhaps, approximately) 
2*pi/Arg(polyroot(c(1, -ar.coef)) )

plot(polyroot(c(1, -my.fit$coef[1])), 
     xlim=c(-1.5,1.5), ylim=c(-1.3,1.3), 
     xlab="Real axis", ylab="Imaginary axis",
     main="Roots of Autoregressive Polynomial", asp=1)
symbols(0, 0, circles=1, add=T, inches=F, col=5) 
##  Make sure to set xlim and ylim big enough

