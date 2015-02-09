library(ISLR)
library(MASS)
library(ggplot2)
library(pryr)
library(dplyr)
library(mvtnorm)

Advertising <- read.csv("Advertising.csv")

par(mfrow=c(1,3))

names(Advertising)

# Fig 2.1
plot(Sales ~ TV, Advertising)
abline(lm(Sales ~ TV, Advertising), col="red")
plot(Sales ~ Radio, Advertising)
abline(lm(Sales ~ Radio, Advertising), col="red")
plot(Sales ~ Newspaper, Advertising)
abline(lm(Sales ~ Newspaper, Advertising), col="red")

Income <- read.csv("Income1.csv")

# Fig 2.2
plot(Income ~ Education, Income)
poly.fit <- lm(Income ~ poly(Education, 4), Income)
points(Income$Education, fitted(poly.fit), col="green")

segments(Income$Education, fitted(poly.fit),
         Income$Education, Income$Income, col="red")

# Similar to Fig 2.8
m1 <- c(0, 0)
m2 <- c(0, 6)
m3 <- c(3, 3)

pts1 <- replicate(10, m1 + rnorm(2))
pts2 <- replicate(10, m2 + rnorm(2))
pts3 <- replicate(10, m3 + rnorm(2))

pts <- cbind(pts1, pts2, pts3)
pts.df <- data.frame(x=pts[1,], y=pts[2,], col=c(rep("1", 10), rep("2", 10), rep("3",10)))

ggplot(data=pts.df, aes(x=x, y=y, color=col)) + geom_point()

km.res <- kmeans(pts.df[c("x", "y")], centers=3)

ggplot(data=data.frame(pts.df, clust=as.factor(km.res$cluster)),
       aes(x=x, y=y, color=clust, shape=col)) +
  geom_point()

# Similar to Fig 2.9

fs <- function(x) 3*sin(x/30 + 5) + 2
x <- seq(from=1, to=130, by=3)
y <- fs(x)
y.n <- y + rnorm(length(x))
y.n.2 <- y + rnorm(length(x))

plot(x,y.n)
lines(x,y)

fit.1 <- lm(y.n ~ x)
lines(x, fitted(fit.1), col="green")

fit.2 <- lm(y.n ~ poly(x, 5))
lines(x, fitted(fit.2), col="red")

fit.3 <- lm(y.n ~ poly(x, 20))
lines(x, fitted(fit.3), col="blue")

fits <- lapply(1:20, function(deg) {lm(y.n ~ poly(x, deg))})
msse <- sapply(fits, function(fit) sum(fit$residual^2)/length(x))

msse.test <- sapply(1:20, function(idx) sum((y.n.2 - predict(fits[[idx]]))^2)/length(x))

ggplot(data.frame(x=1:20, y=msse, y2=msse.test), aes(x=x, y=y)) + geom_line(aes(color="msse.train")) +
  geom_line(aes(x=x, y=y2, color="msse.test"))

## Bayes decision boundary
mu1 <- c(0, 0)
Sigma1 <- matrix(c(1,0,0,1), nrow=2)
mu2 <- c(1, 6)
Sigma2 <- matrix(c(2,0,0,1), nrow=2)

dmvnorm(c(1,1), mu2, Sigma2)

class1p <- function(x) {
  dmvnorm(x, mu1, Sigma1)/(dmvnorm(x, mu1, Sigma1) + dmvnorm(x, mu2, Sigma2))
}

bayes.class <- function(x) {
  if (dmvnorm(x, mu1, Sigma1) > dmvnorm(x, mu2, Sigma2))
    return("class1")
  else
    return("class2")
}
xs <- seq(-12,12,by=1)
ys <- seq(-2,10,by=1)
eg <- expand.grid(xs, ys)
eg <- eg %>% rowwise() %>% mutate(bayesc = bayes.class(c(Var1, Var2)), bayesp = class1p(c(Var1, Var2)))

# the following does really poorly on the contour lines
ggplot(eg, aes(x=Var1, y=Var2, z = bayesp, color=bayesc))  +  stat_contour(breaks=c(0.4,0.5,0.6))

# this draws a nice line, but then need to use base graphics
contour(xs, ys, matrix(eg$bayesp, nrow=length(xs)), levels=c(0.5))
# this call gives you the data for the contour line to be used as you please
contLines <- contourLines(xs, ys, matrix(eg$bayesp, nrow=length(xs)), levels=c(0.5))

ggplot(as.data.frame(contLines), aes(x=x, y=y)) + geom_line() +
  geom_point(data=eg, aes(x=Var1, y=Var2, color=bayesc))
