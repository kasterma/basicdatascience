loss <- function(predictions) {
    predictions$rowidx <- sapply(predictions$X,
                                 f(id, which(train$X==id)))
    y <- train[predictions$rowidx,]$Made.Donation.in.March.2007
    -1/nrow(predictions)*sum(y*log(predictions$p) + (1-y)*log(1-predictions$p))
}
