# Model adequacy checking 
library("sde")
#############    MODEL 1    ###########
k<-3 # indepencent variables 
n<- 1000 # number of observations 
X<-array(0,dim=c(n,(k+1))) 
bt<-c(1,2.3, 1.5,0.05)

x1<-rgamma(n,2,3)
x2<-rbinom(n,10,0.7)
x3<-rbeta(n,0.5,0.5)
X[,1]<-1
X[,2]<-x1
X[,3]<-x2
X[,4]<-x3
sigma<-2
eps<-rnorm(n,0,sigma)
y<-X%*%bt+eps 
d<- data.frame(y,X[,2],X[,3],X[,4])

bth<-lm(y~X[,2]+X[,3]+X[,4],data=d)

cat("trur beta=",bt, "sigma=",sigma,'\n')
cat("beta_hat=",coefficients(bth), "sigma_hat=",summary(bth)$sigma,'\n') # model coefficients

par(mfrow=c(1,2))
hist(bth$residuals, breaks = 100, probability = T )
s<-seq(min(bth$residuals), max(bth$residuals), length=100)
lines(dnorm(s, mean(bth$residuals),sd(bth$residuals))~s, col=2)
plot(predict(bth),bth$residuals)
#qqnorm(y = bth$residuals, plot.it = T )
#qqline(y=bth$residuals, col=2, lwd=2,lty=2)

stdz<-(stdres(bth)) 
hist(stdz, breaks = 100, probability = T )
s<-seq(min(stdz), max(stdz), length=100)
lines(dnorm(s, 0,1)~s, col=2)
qqnorm(y = stdz, plot.it = T )
qqline(y=stdz, col=2, lwd=2,lty=2)
abline(a = 0,b = 1, col=3)


print(ks.test(x=bth$residuals, y="pnorm",alternative = c("two.sided")))
print(ks.test(x=stdz, y="pnorm",alternative = c("two.sided")))




#############    MODEL 2 # Normal with non zero mean     ###########
k<-3 # indepencent variables 
n<- 1000 # number of observations 
X<-array(0,dim=c(n,(k+1))) 
bt<-c(1,2.3, 1.5,0.05)

x1<-rgamma(n,2,3)
x2<-rbinom(n,10,0.7)
x3<-rbeta(n,0.5,0.5)
X[,1]<-1
X[,2]<-x1
X[,3]<-x2
X[,4]<-x3
sigma<-2
eps<-rnorm(n,3,sigma)
y<-X%*%bt+eps 
d<- data.frame(y,X[,2],X[,3],X[,4])

bth<-lm(y~X[,2]+X[,3]+X[,4],data=d)

cat("trur beta=",bt, "sigma=",sigma,'\n')
cat("beta_hat=",coefficients(bth), "sigma_hat=",summary(bth)$sigma,'\n') # model coefficients


par(mfrow=c(1,2))
hist(bth$residuals, breaks = 100, probability = T )
s<-seq(min(bth$residuals), max(bth$residuals), length=100)
lines(dnorm(s, mean(bth$residuals),sd(bth$residuals))~s, col=2)
plot(predict(bth),bth$residuals)
# qqnorm(y = bth$residuals, plot.it = T )
# qqline(y=bth$residuals, col=2, lwd=2,lty=2)

stdz<-(stdres(bth))
hist(stdz, breaks = 100, probability = T )
s<-seq(min(stdz), max(stdz), length=100)
lines(dnorm(s, 0,1)~s, col=2)
qqnorm(y = stdz, plot.it = T )
qqline(y=stdz, col=2, lwd=2,lty=2)
abline(a = 0,b = 1, col=3)


print(ks.test(x=bth$residuals, y="pnorm",alternative = c("two.sided")))
print(ks.test(x=stdz, y="pnorm",alternative = c("two.sided")))




#############    MODEL 3  # Heavy tailed distribution   ###########
k<-3 # indepencent variables 
n<- 1000 # number of observations 
X<-array(0,dim=c(n,(k+1))) 
bt<-c(1,2.3, 1.5,0.05)

x1<-rgamma(n,2,3)
x2<-rbinom(n,10,0.7)
x3<-rbeta(n,0.5,0.5)
X[,1]<-1
X[,2]<-x1
X[,3]<-x2
X[,4]<-x3
#sigma<-2
eps<-rt(n,df = 1)   # Heavy tailed distribution 
y<-X%*%bt+eps 
d<- data.frame(y,X[,2],X[,3],X[,4])


bth<-lm(y~X[,2]+X[,3]+X[,4],data=d)

cat("trur beta=",bt, "sigma=",sigma,'\n')
cat("beta_hat=",coefficients(bth), "sigma_hat=",summary(bth)$sigma,'\n') # model coefficients



par(mfrow=c(1,2))
hist(bth$residuals, breaks = 100, probability = T )
s<-seq(min(bth$residuals), max(bth$residuals), length=100)
lines(dnorm(s, mean(bth$residuals),sd(bth$residuals))~s, col=2)
plot(predict(bth),bth$residuals)
#qqnorm(y = bth$residuals, plot.it = T )
#qqline(y=bth$residuals, col=2, lwd=2,lty=2)

stdz<-(stdres(bth))
hist(stdz, breaks = 100, probability = T )
s<-seq(min(stdz), max(stdz), length=100)
lines(dnorm(s, 0,1)~s, col=2)
qqnorm(y = stdz, plot.it = T )
qqline(y=stdz, col=2, lwd=2,lty=2)
abline(a = 0,b = 1, col=3)


print(ks.test(x=bth$residuals, y="pnorm",alternative = c("two.sided")))
print(ks.test(x=stdz, y="pnorm",alternative = c("two.sided")))



#############    MODEL 4  # thin tailed distribution   ###########
k<-3 # indepencent variables 
n<- 1000 # number of observations 
X<-array(0,dim=c(n,(k+1))) 
bt<-c(1,2.3, 1.5,0.05)

x1<-rgamma(n,2,3)
x2<-rbinom(n,10,0.7)
x3<-rbeta(n,0.5,0.5)
X[,1]<-1
X[,2]<-x1
X[,3]<-x2
X[,4]<-x3
sigma<-2
eps<-rexp(n,rate =500)*(rbinom(n,1,0.5)-0.5)*2  # thin tailed distribution 
y<-X%*%bt+eps 
d<- data.frame(y,X[,2],X[,3],X[,4])


bth<-lm(y~X[,2]+X[,3]+X[,4],data=d)

cat("trur beta=",bt, "sigma=",sigma,'\n')
cat("beta_hat=",coefficients(bth), "sigma_hat=",summary(bth)$sigma,'\n') # model coefficients



par(mfrow=c(1,2))
hist(bth$residuals, breaks = 100, probability = T )
s<-seq(min(bth$residuals), max(bth$residuals), length=100)
lines(dnorm(s, mean(bth$residuals),sd(bth$residuals))~s, col=2)
plot(predict(bth),bth$residuals)
#qqnorm(y = bth$residuals, plot.it = T )
#qqline(y=bth$residuals, col=2, lwd=2,lty=2)

stdz<-(stdres(bth))
hist(stdz, breaks = 100, probability = T )
s<-seq(min(stdz), max(stdz), length=100)
lines(dnorm(s, 0,1)~s, col=2)
qqnorm(y = stdz, plot.it = T )
qqline(y=stdz, col=2, lwd=2,lty=2)
abline(a = 0,b = 1, col=3)


print(ks.test(x=bth$residuals, y="pnorm",alternative = c("two.sided")))
print(ks.test(x=stdz, y="pnorm",alternative = c("two.sided")))








#############    MODEL 5  # Asymmtric distribution ###########
k<-3 # indepencent variables 
n<- 1000 # number of observations 
X<-array(0,dim=c(n,(k+1))) 
bt<-c(1,2.3, 1.5,0.05)

x1<-rgamma(n,2,3)
x2<-rbinom(n,10,0.7)
x3<-rbeta(n,0.5,0.5)
X[,1]<-1
X[,2]<-x1
X[,3]<-x2
X[,4]<-x3
sigma<-2
eps<-rgamma(n,shape = 2, rate = sigma)   # positively skewed 
y<-X%*%bt+eps 
d<- data.frame(y,X[,2],X[,3],X[,4])


bth<-lm(y~X[,2]+X[,3]+X[,4],data=d)

cat("trur beta=",bt, "sigma=",sigma,'\n')
cat("beta_hat=",coefficients(bth), "sigma_hat=",summary(bth)$sigma,'\n') # model coefficients



par(mfrow=c(1,2))
hist(bth$residuals, breaks = 100, probability = T )
s<-seq(min(bth$residuals), max(bth$residuals), length=100)
lines(dnorm(s, mean(bth$residuals),sd(bth$residuals))~s, col=2)
plot(predict(bth),bth$residuals)
#qqnorm(y = bth$residuals, plot.it = T )
#qqline(y=bth$residuals, col=2, lwd=2,lty=2)

stdz<-(stdres(bth))
hist(stdz, breaks = 100, probability = T )
s<-seq(min(stdz), max(stdz), length=100)
lines(dnorm(s, 0,1)~s, col=2)
qqnorm(y = stdz, plot.it = T )
qqline(y=stdz, col=2, lwd=2,lty=2)
abline(a = 0,b = 1, col=3)


print(ks.test(x=bth$residuals, y="pnorm",alternative = c("two.sided")))
print(ks.test(x=stdz, y="pnorm",alternative = c("two.sided")))


#############    MODEL 6  # Asymmtric distribution ###########
k<-3 # indepencent variables 
n<- 1000 # number of observations 
X<-array(0,dim=c(n,(k+1))) 
bt<-c(1,2.3, 1.5,0.05)

x1<-rgamma(n,2,3)
x2<-rbinom(n,10,0.7)
x3<-rbeta(n,0.5,0.5)
X[,1]<-1
X[,2]<-x1
X[,3]<-x2
X[,4]<-x3
sigma<-2
eps<- rgamma(n,shape = 2, rate = sigma)    
y<-X%*%bt-eps  # negatively  skewed
d<- data.frame(y,X[,2],X[,3],X[,4])


bth<-lm(y~X[,2]+X[,3]+X[,4],data=d)

cat("trur beta=",bt, "sigma=",sigma,'\n')
cat("beta_hat=",coefficients(bth), "sigma_hat=",summary(bth)$sigma,'\n') # model coefficients



par(mfrow=c(1,2))
hist(bth$residuals, breaks = 100, probability = T )
s<-seq(min(bth$residuals), max(bth$residuals), length=100)
lines(dnorm(s, mean(bth$residuals),sd(bth$residuals))~s, col=2)
plot(predict(bth),bth$residuals)
#qqnorm(y = bth$residuals, plot.it = T )
#qqline(y=bth$residuals, col=2, lwd=2,lty=2)
stdz<-(stdres(bth))
hist(stdz, breaks = 100, probability = T )
s<-seq(min(stdz), max(stdz), length=100)
lines(dnorm(s, 0,1)~s, col=2)
qqnorm(y = stdz, plot.it = T )
qqline(y=stdz, col=2, lwd=2,lty=2)
abline(a = 0,b = 1, col=3)


print(ks.test(x=bth$residuals, y="pnorm",alternative = c("two.sided")))
print(ks.test(x=stdz, y="pnorm",alternative = c("two.sided")))




#############    MODEL 7  # dependent normal ###########
k<-3 # indepencent variables 
n<- 1000 # number of observations 
X<-array(0,dim=c(n,(k+1))) 
bt<-c(1,2.3, 1.5,0.05)

x1<-rgamma(n,2,3)
x2<-rbinom(n,10,0.7)
x3<-rbeta(n,0.5,0.5)
X[,1]<-1
X[,2]<-x1
X[,3]<-x2
X[,4]<-x3
sigma<-2

eps<-BM(x = 0.1,t0 =0,T = 1,N = (n-1) )   # dependent normal Brownian motion 
y<-X%*%bt+eps 
d<- data.frame(y,X[,2],X[,3],X[,4])


bth<-lm(y~X[,2]+X[,3]+X[,4],data=d)

cat("trur beta=",bt, "sigma=",sigma,'\n')
cat("beta_hat=",coefficients(bth), "sigma_hat=",summary(bth)$sigma,'\n') # model coefficients

par(mfrow=c(1,2))
hist(bth$residuals, breaks = 100, probability = T )
s<-seq(min(bth$residuals), max(bth$residuals), length=100)
lines(dnorm(s, mean(bth$residuals),sd(bth$residuals))~s, col=2)
plot(predict(bth),bth$residuals)
#qqnorm(y = bth$residuals, plot.it = T )
#qqline(y=bth$residuals, col=2, lwd=2,lty=2)
stdz<-(stdres(bth))
hist(stdz, breaks = 100, probability = T )
s<-seq(min(stdz), max(stdz), length=100)
lines(dnorm(s, 0,1)~s, col=2)
qqnorm(y = stdz, plot.it = T )
qqline(y=stdz, col=2, lwd=2,lty=2)
abline(a = 0,b = 1, col=3)


print(ks.test(x=bth$residuals, y="pnorm",alternative = c("two.sided")))
print(ks.test(x=stdz, y="pnorm",alternative = c("two.sided")))




#############    MODEL 8  # wrong model 1  ###########
k<-3 # indepencent variables 
n<- 1000 # number of observations 
X<-array(0,dim=c(n,(k+1))) 
bt<-c(1,2.3, 1.5,0.05)

x1<-rgamma(n,2,3)
x2<-rbinom(n,10,0.7)
x3<-rbeta(n,0.5,0.5)
X[,1]<-1
X[,2]<-x1
X[,3]<-x2
X[,4]<-100*x3*x1^2
sigma<-2


eps<-rnorm(n,0,sigma) 
y<-X%*%bt+eps 
X[,4]<-x3
d<- data.frame(y,X[,2],X[,3],X[,4])


bth<-lm(y~X[,2]+X[,3]+X[,4],data=d)

cat("trur beta=",bt, "sigma=",sigma,'\n')
cat("beta_hat=",coefficients(bth), "sigma_hat=",summary(bth)$sigma,'\n') # model coefficients

par(mfrow=c(1,2))
hist(bth$residuals, breaks = 100, probability = T )
s<-seq(min(bth$residuals), max(bth$residuals), length=100)
lines(dnorm(s, mean(bth$residuals),sd(bth$residuals))~s, col=2)
plot(predict(bth),bth$residuals)
#qqnorm(y = bth$residuals, plot.it = T )
#qqline(y=bth$residuals, col=2, lwd=2,lty=2)
stdz<-(stdres(bth))
hist(stdz, breaks = 100, probability = T )
s<-seq(min(stdz), max(stdz), length=100)
lines(dnorm(s, 0,1)~s, col=2)
qqnorm(y = stdz, plot.it = T )
qqline(y=stdz, col=2, lwd=2,lty=2)
abline(a = 0,b = 1, col=3)


print(ks.test(x=bth$residuals, y="pnorm",alternative = c("two.sided")))
print(ks.test(x=stdz, y="pnorm",alternative = c("two.sided")))



#############    MODEL 9  # wrong model 2  ###########
k<-3 # indepencent variables 
n<- 1000 # number of observations 
X<-array(0,dim=c(n,(k+1))) 
bt<-c(1,2.3, 1.5,0.05)

x1<-rgamma(n,2,3)
x2<-rbinom(n,10,0.7)
x3<-rbeta(n,0.5,0.5)
X[,1]<-1
X[,2]<-x1
X[,3]<-x2
X[,4]<-x3
sigma<-2


eps<-rnorm(n,0,sigma) 
y<-X%*%bt+eps*x3    # error depends on x values 
d<- data.frame(y,X[,2],X[,3],X[,4])


bth<-lm(y~X[,2]+X[,3]+X[,4],data=d)

cat("trur beta=",bt, "sigma=",sigma,'\n')
cat("beta_hat=",coefficients(bth), "sigma_hat=",summary(bth)$sigma,'\n') # model coefficients

par(mfrow=c(1,2))
hist(bth$residuals, breaks = 100, probability = T )
s<-seq(min(bth$residuals), max(bth$residuals), length=100)
lines(dnorm(s, mean(bth$residuals),sd(bth$residuals))~s, col=2)
plot(predict(bth),bth$residuals)
#qqnorm(y = bth$residuals, plot.it = T )
#qqline(y=bth$residuals, col=2, lwd=2,lty=2)

stdz<-(stdres(bth))
hist(stdz, breaks = 100, probability = T )
s<-seq(min(stdz), max(stdz), length=100)
lines(dnorm(s, 0,1)~s, col=2)
qqnorm(y = stdz, plot.it = T )
qqline(y=stdz, col=2, lwd=2,lty=2)
abline(a = 0,b = 1, col=3)


print(ks.test(x=bth$residuals, y="pnorm",alternative = c("two.sided")))
print(ks.test(x=stdz, y="pnorm",alternative = c("two.sided")))


# Motor Trend Car Road Tests
# 
# Description
# 
# The data was extracted from the 1974 Motor Trend US magazine,
# and comprises fuel consumption and 10 aspects of automobile design
# and performance for 32 automobiles (1973–74 models).
# A data frame with 32 observations on 11 variables.
# 
# [, 1]	mpg	   Miles/(US) gallon
# [, 2]	cyl	   Number of cylinders
# [, 3]	disp	 Displacement (cu.in.)
# [, 4]	hp	   Gross horsepower
# [, 5]	drat	 Rear axle ratio
# [, 6]	wt	   Weight (1000 lbs)
# [, 7]	qsec	 1/4 mile time
# [, 8]	vs	   V/S
# [, 9]	am	   Transmission (0 = automatic, 1 = manual)
# [,10]	gear	 Number of forward gears
# [,11]	carb	 Number of carburetors
# Source
# 
# Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391–411.

rsqurd<-array(0,dim = c(10))
adjrsqurd<-array(0,dim = c(10))
fit<-lm(mpg~wt, data = mtcars)
rsqurd[1]<-summary(fit)$r.squared
adjrsqurd[1]<-summary(fit)$adj.r.squared

fit<-lm(mpg~wt+am, data = mtcars)
rsqurd[2]<-summary(fit)$r.squared
adjrsqurd[2]<-summary(fit)$adj.r.squared

fit<-lm(mpg~wt+am+qsec, data = mtcars)
rsqurd[3]<-summary(fit)$r.squared
adjrsqurd[3]<-summary(fit)$adj.r.squared

fit<-lm(mpg~wt+am+qsec+hp, data = mtcars)
rsqurd[4]<-summary(fit)$r.squared
adjrsqurd[4]<-summary(fit)$adj.r.squared

fit<-lm(mpg~wt+am+qsec+hp+disp, data = mtcars)
rsqurd[5]<-summary(fit)$r.squared
adjrsqurd[5]<-summary(fit)$adj.r.squared


fit<-lm(mpg~wt+am+qsec+hp+disp+drat, data = mtcars)
rsqurd[6]<-summary(fit)$r.squared
adjrsqurd[6]<-summary(fit)$adj.r.squared


fit<-lm(mpg~wt+am+qsec+hp+disp+drat+gear, data = mtcars)
rsqurd[7]<-summary(fit)$r.squared
adjrsqurd[7]<-summary(fit)$adj.r.squared

fit<-lm(mpg~wt+am+qsec+hp+disp+drat+gear+carb, data = mtcars)
rsqurd[8]<-summary(fit)$r.squared
adjrsqurd[8]<-summary(fit)$adj.r.squared



fit<-lm(mpg~wt+am+qsec+hp+disp+drat+gear+carb+vs, data = mtcars)
rsqurd[9]<-summary(fit)$r.squared
adjrsqurd[9]<-summary(fit)$adj.r.squared

fit<-lm(mpg~wt+am+qsec+hp+disp+drat+gear+carb+vs+cyl, data = mtcars)
rsqurd[10]<-summary(fit)$r.squared
adjrsqurd[10]<-summary(fit)$adj.r.squared

par(mfrow=c(1,1))
plot(rsqurd, type='b', ylim=c(0.7,0.88), main="R^2 (black) & adj R^2 (red)")
lines(adjrsqurd,col=2,type = 'b')


