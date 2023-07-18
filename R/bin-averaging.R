#' Bin average from index positions
#'
#' Calculate bin averages defined by vector index positions.
#'
#' Averaging bins are defined consecutively from the first index position
#' to the index position one before the next index, i.e., closed on the left
#' and open on the right (the right end of the first bin does not overlap with
#' the left end of the second bin, and so on).
#'
#' But note that the final bin is treated separately: if the last index
#' coincides with the last data point of \code{x}, this point _is_ included in
#' the last bin average, else the bin is treated analogously to the other bins
#' (see examples).
#'
#' @param x numeric vector for which to compute bin averages according to the
#' indices given in \code{ind}.
#' @param ind numeric vector of index positions specifying the binning windows
#'   to average over, see details.
#' @param na.rm a logical value indicating whether ‘NA’ values should be
#' stripped before the computation proceeds. Defaults to \code{TRUE}.
#' @return a numeric vector of length \code{length(ind) - 1} with the mean
#' values of each bin from the break points defined by \code{ind}.
#' @author Thomas Münch
#' @examples
#' x <- rnorm(20)
#'
#' # final bin includes final data point:
#' AverageByIndex(x, ind = c(1, 7, 13, 17, 20))
#' # final index is not final data point:
#' AverageByIndex(x, ind = c(1, 7, 13, 17))
#' @export
AverageByIndex <- function(x, ind, na.rm = TRUE) {

  n <- length(ind)

  # check validity of indices
  if (any(ind < 1)) {
    stop("Indices all must be >= 1.")
  }
  if (min(diff(ind)) < 0) {
    stop("Indices must be monotonically increasing.")
  }
  if (any(ind > length(x))) {
    stop("Index out of data index range.")
  }

  # if final index is final data point, include it in the last bin average
  if (ind[n] == length(x)) {
    ind[n] <- ind[n] + 1
  }
  
  avg <- vector()
  for (i in (1 : (n - 1))) {

    # define bins closed on the left and open on the right
    range <- ind[i] : (ind[i + 1] - 1)
    avg[i] <- mean(x[range], na.rm = na.rm)
  }
  
  return(avg)
}
