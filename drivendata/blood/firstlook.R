library(devtools)
library(dplyr)
library(pryr)
library(caret)
library(GGally)
library(ggplot2)
library(gmodels)

library(devtools)
devtools::load_all()

test <- tbl_df(read.csv("data/test.csv"))
train <- tbl_df(read.csv("data/train.csv"))

## Note: volume donated is a direct function of numer of donations
## drop one of the two
train$Total.Volume.Donated..c.c.. <- NULL

GGally::ggpairs(train[,2:5])

ggplot(train, aes(x=Number.of.Donations)) + geom_freqpoly(binwidth=1)
table(train$Number.of.Donations, train$Made.Donation.in.March.2007)

CrossTable(train$Number.of.Donations, train$Made.Donation.in.March.2007)

## predicting 0.5 everywhere

predeven <- data.frame(X=train$X, p=0.5)
loss(predeven)

## predicting the mean everywhere

(mean <- mean(train$Made.Donation.in.March.2007))
predmean <- data.frame(X=train$X, p=mean)
loss(predmean)

predmean.test <- data.frame(X=test$X, p=mean)

## preparing this for submission to see I know how to submit
write.csv(setNames(predmean.test, c("","Made Donation in March 2007")),
          "meansubmission.csv",
          row.names=FALSE)

