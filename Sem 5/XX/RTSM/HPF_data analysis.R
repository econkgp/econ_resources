w<-read.csv("hpf.csv")
w1<-w[c(-10,-39),]

x <- w1$P
y <- w1$F
z <- w1$H
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

bh<-fit$coefficients

h39<-sum(bh*c(1,18,25.2))
