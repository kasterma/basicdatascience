library(dplyr)
library(pryr)
library(caret)
library(GGally)
library(ggplot2)
library(gmodels)
library(lubridate)

library(devtools)
devtools::load_all()

# first look at data
sampleSubmission <- tbl_df(read.csv("data/sampleSubmission.csv", stringsAsFactors=FALSE))
str(sampleSubmission)
dat.test <- tbl_df(read.csv("data/test.csv", stringsAsFactors=FALSE))
str(dat.test)
nrow(dat.test)  # 6,493
dat.train <- tbl_df(read.csv("data/train.csv", stringsAsFactors=FALSE))
str(dat.train)
nrow(dat.train)   # 10,886

# enrich data
dat.train <- within(dat.train, {tod <- as.factor(hour(ymd_hms(datetime)))
                                dow <- as.factor(wday(ymd_hms(datetime)))})
dat.test <- within(dat.test, {tod <- as.factor(hour(ymd_hms(datetime)))
                              dow <- as.factor(wday(ymd_hms(datetime)))})

# look at some random rows
dat.train[sample(nrow(dat.train), size=10),]
table(dat.train$temp < dat.train$atemp)
table(dat.train$temp == dat.train$atemp)
table(dat.train$temp > dat.train$atemp)

hist(dat.train$humidity, breaks = seq(101)-1)

ggpairs(dat.train[c("temp", "atemp")])

casual.fit <- lm(casual ~ tod + dow + season + holiday + workingday + weather + temp + atemp + humidity + windspeed, dat.train)
casual.preds <- predict(casual.fit, newdat=dat.test)
ggplot(rbind(data.frame(cts=casual.preds, lab="pred"), data.frame(cts=dat.train$casual, lab="real")), aes(x=cts)) + geom_freqpoly() + facet_grid(lab ~.)

registered.fit <- lm(registered ~ tod + dow + season + holiday + workingday + weather + temp + atemp + humidity + windspeed, dat.train)
registered.preds <- predict(registered.fit, newdat=dat.test)
ggplot(rbind(data.frame(cts=registered.preds, lab="pred"), data.frame(cts=dat.train$registered, lab="real")), aes(x=cts)) + geom_freqpoly() + facet_grid(lab ~.)

count.fit <- lm(count ~ tod + dow + season + holiday + workingday + weather + temp + atemp + humidity + windspeed, dat.train)
count.preds <- predict(count.fit, newdat=dat.test)
ggplot(rbind(data.frame(cts=count.preds, lab="pred"), data.frame(cts=dat.train$count, lab="real")), aes(x=cts)) + geom_freqpoly() + facet_grid(lab ~.)

ggplot(rbind(data.frame(cts=casual.preds, labx="pred", laby="casual"),
             data.frame(cts=dat.train$casual, labx="real", laby="casual"),
             data.frame(cts=registered.preds, labx="pred", laby="registered"),
             data.frame(cts=dat.train$registered, labx="real", laby="registered"),
             data.frame(cts=count.preds, labx="pred", laby="count"),
             data.frame(cts=dat.train$count, labx="real", laby="count")),
       aes(x=cts)) + geom_freqpoly(binwidth=1) + facet_grid(labx ~ laby)

table(dat.train$casual)

sampleSubmission$count <- count.preds
sampleSubmission$count <- sapply(sampleSubmission$count, f(x, round(max(0,x))))

write.csv(sampleSubmission, "linear.csv", row.names=FALSE)

# get folds for cross validataion
cv.folds <- createFolds(dat.train$count, k=10)

run.on.fold <- function(dat, test.idx) {
    train.dat <- dat[-test.idx,]
    test.dat <- dat[test.idx,]
    fit <- lm(count ~ tod + dow + season + holiday + workingday + weather + temp + atemp + humidity + windspeed,
              train.dat)
    pred <- sapply(predict(fit, newdata = test.dat),
                   f(x, round(max(0,x))))
    loss(pred, test.dat$count)
}

sapply(seq_along(cv.folds), f(idx, run.on.fold(dat.train, cv.folds[[idx]])))
