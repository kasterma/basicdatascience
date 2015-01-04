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

test <- tbl_df(read.csv("data/test.csv",
                        colClasses=c("integer",
                            "numeric", "numeric", "numeric", "numeric")))
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

run.on.fold <- function(dat, fold) {
    train.fold <- dat[fold,]
    test.fold <- dat[-fold,]
    fit <- lm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Months.since.First.Donation + Number.of.Donations, train.fold)
#    print(fit$coefficients)
    pred <- predict(fit, test.fold)
    predlm <- data.frame(X=test.fold$X,
                         p=ifelse(pred < 1, ifelse(pred > 0, pred, 0.0001), 0.9999))
    list(loss=loss(predlm),coeff=fit$coefficients,out.of.bounds=sum(pred < 0 | pred > 1),
         pred=pred, X=test.fold$X)
}

cv.res <- lapply(seq_along(train.cv.dat), f(idx, run.on.fold(train, train.cv.dat[[idx]])))
cv.losses <- sapply(cv.res, f(x, x$loss))
mean(cv.losses)

preProcValues <- preProcess(as.data.frame(train[,2:4]), method=c("scale", "center", "BoxCox"))

train.pp <- predict(preProcValues, as.data.frame(train[,2:4]))

train.bc <- data.frame(X=train$X, train.pp, Made.Donation.in.March.2007=train$Made.Donation.in.March.2007)



cv.res.bc <- lapply(seq_along(train.cv.dat), f(idx, run.on.fold(train.bc, train.cv.dat[[idx]])))
cv.bc.losses <- sapply(cv.res.bc, f(x, x$loss))
mean(cv.bc.losses)

## Compare the results
boxplot(res ~ id, data=rbind(data.frame(id=1,res=cv.losses),
                      data.frame(id=2,res=cv.bc.losses)))

xx <- melt(data.frame(id=seq_along(cv.res),cv.losses, cv.bc.losses), id.vars="id")
ggplot(xx, aes(x=id, y=value, color=variable)) + geom_point()

boxplot(res ~ id, data=rbind(data.frame(id="plain",res=cv.res[c(-3)]),
                      data.frame(id="bc",res=cv.res.bc[c(-3)])))





yy <- data.frame(id=1:10, t(rbind(sapply(cv.res, f(x, x$coeff)), sapply(cv.res.bc, f(x, x$coeff)))))
names(yy) <- c("id", "a", "b", "c", "d", "a1", "b1", "c1", "d1")

ggplot(yy, aes(x=d, y=d1)) + geom_point() + geom_text(aes(label=id), hjust=2)

GGally::ggpairs(yy[,-1])

zz <- data.frame(id=seq_along(train.cv.dat),
                 m1=sapply(seq_along(train.cv.dat), f(idx, mean(train$Number.of.Donations[-train.cv.dat[[idx]]]))),
                 m2=sapply(seq_along(train.cv.dat), f(idx, mean(train.bc$Number.of.Donations[-train.cv.dat[[idx]]]))))
zz <- melt(zz, id.vars = "id")

ggplot(zz, aes(x=id, y=value, color=variable)) + geom_point()

zz <- data.frame(id=seq_along(train.cv.dat),
                 m1=sapply(seq_along(train.cv.dat), f(idx, mean(train$Months.since.Last.Donation[-train.cv.dat[[idx]]]))),
                 m2=sapply(seq_along(train.cv.dat), f(idx, mean(train.bc$Months.since.Last.Donation[-train.cv.dat[[idx]]]))))
zz <- melt(zz, id.vars = "id")

ggplot(zz, aes(x=id, y=value, color=variable)) + geom_point()

zz <- data.frame(id=seq_along(train.cv.dat),
                 m1=sapply(seq_along(train.cv.dat), f(idx, mean(train$Months.since.First.Donation[-train.cv.dat[[idx]]]))),
                 m2=sapply(seq_along(train.cv.dat), f(idx, mean(train.bc$Months.since.First.Donation[-train.cv.dat[[idx]]]))))
zz <- melt(zz, id.vars = "id")

ggplot(zz, aes(x=id, y=value, color=variable)) + geom_point()

sum(sapply(cv.res, f(x, x$out.of.bounds)))
sum(sapply(cv.res.bc, f(x, x$out.of.bounds)))

# boxplot comparint a statistic of the results
stat.bp <- function(sf) {
    ms <- rbind(data.frame(id="cv", dat=sapply(cv.res, f(x, sf(x$pred)))),
                data.frame(id="bc", dat=sapply(cv.res.bc, f(x, sf(x$pred)))))
    boxplot(dat ~ id, ms)
}

stat.bp(mean)

# get list of confident wrong
# those that individually have large contibution to loss

bound.pred <- function(res) {
    data.frame(X=res$X,
               p=ifelse(res$pred < 1, ifelse(res$pred > 0, res$pred, 0.0001), 0.9999))
}

hist(bound.pred(cv.res.bc[[1]])[,2])

ss <- Reduce(rbind,
             lapply(seq_along(cv.res.bc),
                    f(idx, data.frame(id=idx, lab="bc", bound.pred(cv.res.bc[[idx]])))))
ss2 <- Reduce(rbind,
              lapply(seq_along(cv.res),
                     f(idx, data.frame(id=idx, lab="cv", bound.pred(cv.res[[idx]])))))
ssa <- rbind(ss, ss2)

ggplot(ssa, aes(x=p)) + facet_grid(id ~ lab) + geom_freqpoly()
