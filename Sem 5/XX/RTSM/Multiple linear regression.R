####################################
# Multiple linear regression 
####################################
# Motor Trend Car Road Tests
# 
# Description
# 
# The data was extracted from the 1974 Motor Trend US magazine,
# and comprises fuel consumption and 10 aspects of automobile design
# and performance for 32 automobiles (1973-74 models).
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
# Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391-411.

print(mtcars[,c("cyl","disp","hp","drat","wt")])
summary(mtcars[,c("cyl","disp","hp","drat","wt")])
plot((mtcars[,c("cyl","disp","hp","drat","wt")]))

fit1<-lm(mpg~cyl+disp+hp+drat+wt, data = mtcars)
summary(fit1)

fit2<-lm(mpg~hp+wt, data = mtcars)
summary(fit2)



x <- mtcars$wt
y <- mtcars$hp
z <- mtcars$mpg
fit <- lm(z ~ x + y)
grid.lines = 40
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit)
library("plot3D")
scatter3D(x, y, z, pch = 1, cex = 1, theta = 20, phi = 40, ticktype = "detailed",xlab = "wt", ylab = "hp", zlab = "mpg",  surf = list(x = x.pred, y = y.pred, z = z.pred,  facets = NA, fit = fitpoints), main = "mtcars")
library("plot3Drgl")
plotrgl()



#ANOVA
# Regression with qualitative predictive variables
# 
n <- 200
x <- sample(0:1, n, replace=T)
y <- 2*(x==0) +rnorm(n)
plot( y ~ factor(x), 
      horizontal = TRUE, 
      xlab = 'y', 
      ylab = 'x',
      col = "pink" )

plot(density(y[x==0]), 
     lwd = 3, 
     xlim = c(-3,5), 
     ylim = c(0,.5), 
     col = 'blue',
     main = "Density in each group",
     xlab = "x")
lines(density(y[x==1]), 
      lwd = 3, 
      col = 'red')

plot(y ~ x,
     main = "lm(y~x) when x is qualitative")
abline(lm(y ~ x), 
       col = 'red')

anova( lm(y~x))
tapply(y,x,mean)


