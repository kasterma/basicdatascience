## Adding preprocessing to the lm with CV model

library(devtools)
library(reshape)
library(plyr)
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


test <- tbl_df(read.csv("data/test.csv",
                        colClasses=c("integer",
                            "numeric", "numeric", "numeric", "numeric",
                            "numeric")))
train <- tbl_df(read.csv("data/train.csv",
                         colClasses=c("integer",
                             "numeric", "numeric", "numeric", "numeric",
                             "numeric")))

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

preProcValues <- preProcess(as.data.frame(train[,2:4]), method=c("BoxCox"))

train.pp <- predict(preProcValues, as.data.frame(train[,2:4]))

train <- data.frame(X=train$X, train.pp, Made.Donation.in.March.2007=train$Made.Donation.in.March.2007)

run.on.fold <- function(fold) {
    train.fold <- train[fold,]
    test.fold <- train[-fold,]
    fit <- lm(Made.Donation.in.March.2007 ~ ., train.fold)
    pred <- predict(fit, test.fold)
    predlm <- data.frame(X=test.fold$X,
                         p=ifelse(pred < 1, ifelse(pred > 0, pred, 0.0001), 0.9999))
    loss(predlm)
}


(cv.res.bc <- sapply(seq_along(train.cv.dat), f(idx, run.on.fold(train.cv.dat[[idx]]))))
mean(cv.res.bc)

## Compare the results
boxplot(res ~ id, data=rbind(data.frame(id=1,res=cv.res),
                      data.frame(id=2,res=cv.res.bc)))

xx <- melt(data.frame(id=seq_along(cv.res),cv.res, cv.res.bc), id.vars="id")
ggplot(xx, aes(x=id, y=value, color=variable)) + geom_point()

boxplot(res ~ id, data=rbind(data.frame(id="plain",res=cv.res[c(-3)]),
                      data.frame(id="bc",res=cv.res.bc[c(-3)])))


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
