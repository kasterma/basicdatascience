# following along with:
# https://highlyscalable.wordpress.com/2012/05/01/probabilistic-structures-web-analytics-data-mining/

library(ggplot2)
library(dplyr)

dat <- sample(seq(from=1, to=10^6), replace=TRUE, size=10^7)

## sanity check on the data
tdat <- table(dat)
max(names(tdat))
min(names(tdat))
hist(tdat)
x <- seq(0,25)    # show fit with binomial distr
y <- dbinom(x, size=10^7, prob=1/(10^6))
lines(x, y*10^6)
mean(tdat)  # Note: slightly high since 0's are not counted
sum(tdat)
length(table(dat))
rm(tdat)

format(object.size(dat), units="MB")  # 40000040 bytes = 38.1 MB

## less balanced data
dat2 <- sample(seq(from=1, to=10^6), replace=TRUE, size=10^7, prob=1/seq(1,10^6))
ggplot(dat=data.frame(x=dat2), aes(x=x)) + geom_freqpoly()
max(table(dat2))
length(table(dat2))


format(object.size(unname(tdat)), units="MB")  # 3.8 MB
format(object.size(unname(table(dat2))), units="MB") # 2.9 MB

format(object.size(unique(dat)), units="MB") # 3.8 MB

format(object.size(as.character(dat)), units="MB") # 122 MB
format(object.size(as.character(unique(dat))), units="MB") # 53 MB

format(object.size(tdat), units="MB") # 57 MB

## In native table implementation R converts the labels for the table to
## strings, hence the size discrepancies.

tab3 <- data.frame(x=dat, ct=1) %>% group_by(x) %>% summarize(as.integer(sum(ct)))
format(object.size(tab3), units="MB") # 7.6MB


## Linear counting
#
# Can get bit vectors in R though package bit
library(bit)

object.size(bit(10000))
object.size(bit(20000))
object.sizt(bit(40000))
# object size seems to be #bits needed + 1350