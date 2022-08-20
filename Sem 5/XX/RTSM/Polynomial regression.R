# Polynomial  regression 
####################################
k<-3 # Polynomial degree 
n<- 50 # number of observations 
X<-array(0,dim=c(n,(k+1))) 
bt<-c(1,2.3, -1.5,0.05)

x1<-sort(runif(n,-3,6))
x2<-x1^2
x3<-x1^3
X[,1]<-1
X[,2]<-x1
X[,3]<-x2
X[,4]<-x3
eps<-rnorm(n,0,1)
y<-X%*%bt+eps ##### OR  y<-bt[1]+bt[2]*x1+bt[3]*x2+bt[4]*x3+eps
d<- data.frame(y,x1,x2,x3)
write.table(d,"data.txt")

print(d[1:10,])
plot(y~x1)
s<-seq(min(x1), max(x1),length=100)
lines(bt[1]+bt[2]*s+bt[3]*s^2+bt[4]*s^3~s,col=2, lwd=4, lty=1)


plot(y~x1)
s<-seq(min(x1), max(x1),length=100)
lines(bt[1]+bt[2]*s+bt[3]*s^2+bt[4]*s^3~s,col=2, lwd=4, lty=1)
bth<-lm(y~x1,data=d)
lines(bth$coefficients[1]+bth$coefficients[2]*s~s,col=3, lwd=2, lty=1)



cat("trur beta=",bt, "sigma=",1,'\n')
cat("beta_hat=",coefficients(bth), "sigma_hat=",summary(bth)$sigma,'\n') # model coefficients
print(summary(bth))

plot(y~x1)
s<-seq(min(x1), max(x1),length=100)
lines(bt[1]+bt[2]*s+bt[3]*s^2+bt[4]*s^3~s,col=2, lwd=4, lty=1)
bth<-lm(y~x1,data=d)
lines(bth$coefficients[1]+bth$coefficients[2]*s~s,col=3, lwd=2, lty=1)
bth<-lm(y~x1+x2,data=d)
lines(bth$coefficients[1]+bth$coefficients[2]*s+bth$coefficients[3]*s^2~s,col=4, lwd=2, lty=1)

cat("trur beta=",bt, "sigma=",1,'\n')
cat("beta_hat=",coefficients(bth), "sigma_hat=",summary(bth)$sigma,'\n') # model coefficients
print(summary(bth))



plot(y~x1)
s<-seq(min(x1), max(x1),length=100)
lines(bt[1]+bt[2]*s+bt[3]*s^2+bt[4]*s^3~s,col=2, lwd=4, lty=1)
bth<-lm(y~x1,data=d)
lines(bth$coefficients[1]+bth$coefficients[2]*s~s,col=3, lwd=2, lty=1)
bth<-lm(y~x1+x2,data=d)
lines(bth$coefficients[1]+bth$coefficients[2]*s+bth$coefficients[3]*s^2~s,col=4, lwd=2, lty=1)
bth<-lm(y~x1+x2+x3,data=d)
lines(bth$coefficients[1]+bth$coefficients[2]*s+bth$coefficients[3]*s^2+bth$coefficients[4]*s^3~s,col=5, lwd=2, lty=1)


legend(4, 2, legend=c( "true", "dg=1", "dg=2", "dg=3"),
       col=c(2:5), lty=c(1,1,1,1), cex=0.8)

cat("trur beta=",bt, "sigma=",1,'\n')
cat("beta_hat=",coefficients(bth), "sigma_hat=",summary(bth)$sigma,'\n') # model coefficients
print(summary(bth))

# Orthogonal polynomial regression

set.seed(123)
n<- 100 # number of observations 
bt<-c(3, 2 , -3, 0, .7)
k<-length(bt)-1   # indepencent variables
X<-array(1,dim=c(n,(k+1))) 
XO<-array(1,dim=c(n,(k+1)))
sigma<-1
eps<-rnorm(n,0,sigma)
x<-sort(runif(n, -3,3))
XM<-as.matrix(poly(x,degree = k, raw = T))
X[,2:(k+1)]<-XM[,1:k]
z<-X%*%bt
y<-z+eps

fit <- lm(y~poly(x,1,raw=T))
sumfit<-summary(fit)
cat("R-Square",sumfit$r.squared, "R-Square",sumfit$adj.r.squared,"\n")
print(fit$coefficients)
fit <- lm(y~poly(x,2,raw=T))
cat("R-Square",sumfit$r.squared, "R-Square",sumfit$adj.r.squared,"\n")
print(fit$coefficients)
fit <- lm(y~poly(x,3,raw=T))
cat("R-Square",sumfit$r.squared, "R-Square",sumfit$adj.r.squared,"\n")
print(fit$coefficients)
fit <- lm(y~poly(x,4,raw=T))
cat("R-Square",sumfit$r.squared, "R-Square",sumfit$adj.r.squared,"\n")
print(fit$coefficients)


fitortho<- lm(y~poly(x,1))
print(fitortho$coefficients)
fitortho<- lm(y~poly(x,2))
print(fitortho$coefficients)
fitortho<- lm(y~poly(x,3))
print(fitortho$coefficients)
fitortho<- lm(y~poly(x,4))
print(fitortho$coefficients)



par(mfrow=c(1,1))
plot(y~x)
par(mfrow=c(1,1))
plot(y~x)
#z<-X%*%bt
lines(z~x, col=2, lwd=2)

z1<-X%*%fit$coefficients
par(mfrow=c(1,1))
plot(y~x)
lines(z~x, col=2, lwd=2)
lines(z1~x, col=3, lwd=2)


XX<-as.matrix(poly(x,k,raw=F))
XO[,2:(k+1)]<-XX[,1:k]
#cf<-fitortho$coefficients[1:(k+1)]
z2<-XO%*%fitortho$coefficients
par(mfrow=c(1,1))
plot(y~x)
lines(z~x, col=2, lwd=2)
lines(z1~x, col=3, lwd=2)
lines(z2~x, col=4, lwd=2,lty=2)
legend(-1, 25, legend=c( "true", "P_4", "Ortho_P_4"),
       col=c(2:4), lty=c(1,1,2), cex=0.8)



