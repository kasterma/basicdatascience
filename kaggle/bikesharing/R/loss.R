#' loss
#' The loss function for this competition
#'
#' Submissions are evaluated one the Root Mean Squared Logarithmic Error
#' (RMSLE). The RMSLE is calculated as
#'
#'      \sqrt{\frac{1}{n} \sum_{i=1}^n (\log(p_i + 1) - \log(a_i+1))^2 }
#'
#' Where:
#'    n is the number of hours in the test set
#'    p_i is your predicted count
#'    a_i is the actual count
#'    log(x) is the natural logarithm
#'
loss <- function(pred.counts, counts) {
    stopifnot(length(pred.counts) == length(counts))
    sqrt(1/length(counts) * sum((log(pred.counts + 1) - log(counts + 1))**2))
}
