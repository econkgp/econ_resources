####################################
# Simple linear regression 
####################################
N <- 100
b0 <- 1
b1 <- -1
X <- runif(N,2,3)
epsilon <- rnorm(N, 0,1)
Y <- b0 + b1 * X + epsilon
SLR<-data.frame(x=X,y=Y)
write.csv(x = SLR, file = "SLR.csv",row.names = F)

slrdata<-read.csv(file = "SLR.csv",header = T)
rr<-lm(slrdata$y~slrdata$x, data = slrdata)
print(rr)
plot(slrdata$y~slrdata$x)
abline(b0,b1, lty=2, col=3, lwd=3)
abline(rr$coefficients[1],rr$coefficients[2], col="red", lwd=3)

print(summary(rr))
plot(rr)


# Distributions of  estimated parameters

itrn<-5000
rcoef<-array(0,dim=c(itrn,2))
rresdsq<-array(0,dim=c(itrn))
for(i in 1: itrn ){
  epsilon <-  runif(N,-1,1)            #rnorm(N, 0,1)
  Y <- b0 + b1 * X +  epsilon
  r<-lm(Y~X)
  rcoef[i,]<-r$coefficients
  rresdsq[i]<-sum((r$residuals)^2)
}
hist(rcoef[,1], probability = T, col=8)
lines(density(rcoef[,1]), col=2, lwd=2)
abline(v=b0, col=3)
hist(rcoef[,2], probability = T, col=6)
lines(density(rcoef[,2]), col=2, lwd=2)
abline(v=b1, col=3)
hist(rresdsq, probability = T, col=7)
lines(density(rresdsq), col=2)


