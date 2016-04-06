loftusMassonStdErrs <- function(dat) {
  # Computes standard errors adjusted to remove within-subject variance (c.f. Loftus & Masson, 1994)
  #
  # Args:
  #  dataframe with:
  #     Each participant in a separate row,
  #     Each condition in a separate column
  #     E.g.:
  #     ConditionA  | ConditionB  | ConditionC    | ConditionD
  #     0.91        | 0.34        | 0.38          | 0.83
  #     0.45        | 0.96        | 0.57          | 0.94
  #     0.68        | 0.79        | 0.58          | 0.98
  #
  # Returns:
  #  The adjusted Std Err for each of the conditions.
  
  library(Matrix)
  dat <- as.matrix(dat)
  diffFromMean <-
    kronecker(matrix(1, 1, ncol(dat)), rowMeans(dat)) - matrix(mean(dat), nrow(dat),  ncol(dat))
  adjustedDat = dat - diffFromMean
  adjustedStdErr <-
    apply(adjustedDat, 2, sd) / sqrt(apply(adjustedDat, 2, function(adjustedDat)
      sum(length(which(
        !is.na(adjustedDat)
      )))))
  # This complicated line does something simple: StdErr = stdDev(x) / sqrt(sampleSize)
  return(adjustedStdErr)
}
