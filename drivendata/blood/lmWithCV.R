## Doing a simple linear model prediction; but use CV to get a decent
## idea of performance before submitting.
##
## Do this CV through caret.

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

fit <- lm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
              Number.of.Donations + Months.since.First.Donation, train)
pred <- predict(fit, train)
predlm <- data.frame(X=train$X, p=ifelse(pred < 1, ifelse(pred > 0, pred, 0.0001), 0.9999))
loss(predlm)

train.cv.dat <- createFolds(train$Made.Donation.in.March.2007, k = 10, returnTrain=TRUE)
str(train.cv.dat)
str(train)

run.on.fold <- function(fold) {
    train.fold <- train[fold,]
    test.fold <- train[-fold,]
    fit <- lm(Made.Donation.in.March.2007 ~ ., train.fold)
    pred <- predict(fit, test.fold)
    predlm <- data.frame(X=test.fold$X,
                         p=ifelse(pred < 1, ifelse(pred > 0, pred, 0.0001), 0.9999))
    loss(predlm)
}

(cv.res <- sapply(seq_along(train.cv.dat), f(idx, run.on.fold(train.cv.dat[[idx]]))))
mean(cv.res)

test.predict <- function(pred.fn) {
    pred <- predict(pred.fn, test)
    setNames(data.frame(test$X,
                        ifelse(pred < 1, ifelse(pred > 0, pred, 0.0001), 0.9999)),
             c("","Made Donation in March 2007"))
}

lmwithcv <- test.predict(lm(Made.Donation.in.March.2007 ~ ., train))

write.csv(lmwithcv,
          "lmwithcv.csv",
          row.names=FALSE)

## results were better than mean and median of the cross validation results
