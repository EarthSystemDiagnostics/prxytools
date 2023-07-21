# Code adapted from mSTEM::which.peaks, which is licensed under
# GPL-3
# Copyright (C) 2019 Zhibing He

# Changes to original code by Thomas Muench on 2023/07/21:
# - function renamed to 'LocatePeaks'
# - function argument 'decreasing' renamed to 'minima'

#' Find local maxima or minima
#'
#' Find the positions of local maxima or minima in a sequence.
#'
#' @param x numeric vector for which maxima (minima) shall be analysed.
#' @param minima if \code{FALSE} (the default) locate the positions of the
#'   maxima of \code{x}, else locate the positions of the minima.
#' @param partial if \code{TRUE}, the first and last data points of \code{x}
#'   are considered to be maxima or minima as well; defaults to \code{FALSE}.
#' @return a numeric vector of the index positions of the maxima (minima).
#' @author Zhibing He, Dan Cheng
#' @source This function is adapted from the function \code{which.peaks} in
#'   the R package \code{mSTEM}:
#'   <https://cran.r-project.org/web/packages/mSTEM/index.html>
#'
#'   For the purpose of the prxytools package, the function and its function
#'   arguments have been renamed.
#' @export
LocatePeaks <- function(x, minima = FALSE, partial = FALSE) {

  x.diff <- diff(x)
  
  if (minima) {

    if (partial) {

      which(diff(c(FALSE, x.diff > 0, TRUE)) > 0)

    } else {

      which(diff(x.diff > 0) > 0) + 1

    }

  } else {

    if (partial) {

      which(diff(c(TRUE, x.diff >= 0, FALSE)) < 0)

    } else {

      which(diff(x.diff >= 0) < 0) + 1
    }
  }
}
