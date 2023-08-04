# Code taken from function Lag in package Hmisc v5.1.0:
# GPL (>=2)
# Copyright (C) 2001 Frank E Harrel Jr

#' Lag a numeric, character, or factor vector
#'
#' Shifts a vector \code{shift} elements later. Character or factor variables
#' are padded with \code{""}, numerics with \code{NA}. The shift may be
#' negative.
#'
#' Attributes of the original object are carried along to the new lagged one.
#'
#' @param x a vector
#' @param shift integer specifying the number of observations to be shifted to
#'   the right. Negative values imply shifts to the left.
#' @return a vector like \code{x}.
#' @seealso \code{\link[stats]{lag}}
#' @author Frank E Harrel Jr
#' @source <https://CRAN.R-project.org/package=Hmisc>
#' @examples
#'
#' Lag(1:5,2)
#' Lag(letters[1:4],2)
#' Lag(factor(letters[1:4]),-2)
#' # Find which observations are the first for a given subject
#' id <- c('a','a','b','b','b','c')
#' id != Lag(id)
#' !duplicated(id)
#'
#' @export
Lag <- function (x, shift = 1) {
  xLen <- length(x)
  if (shift == 0) 
    return(x)
  ret <- as.vector(character(xLen), mode = storage.mode(x))
  attrib <- attributes(x)
  if (length(attrib$label)) 
    attrib$label <- paste(attrib$label, "lagged", shift, 
                          "observations")
  if (abs(shift) < xLen) {
    if (shift > 0) 
      ret[-(1:shift)] <- x[1:(xLen - shift)]
    else ret[1:(xLen + shift)] <- x[(1 - shift):xLen]
  }
  attributes(ret) <- attrib
  return(ret)
}
