#' Check for equidistant resolution
#'
#' This function checks whether a numeric vector (e.g. a depth vector or from a
#' time series) has equidistant increments, i.e. a constant resolution. This
#' check includes a numerical tolerance that accounts for the machine
#' respresentation of floating-point numbers, which circumvents the problems
#' popular methods of checking equidistance have which use, e.g., \code{sd()} or
#' \code{unique()} on the difference vector of \code{x}.
#'
#' @param x a numeric vector.
#' @return a logical value: \code{TRUE} if \code{x} has constant resolution,
#'   \code{FALSE} otherwise.
#' @author Andrew Dolman, Thomas Münch
#' @examples
#'
#' is.equidistant(1 : 10)
#' is.equidistant(c(2.5, 5, 7.5, 10, 12.5))
#' is.equidistant(x <- seq(0, 12, 0.1)) # compare this to sd(diff(x)) == 0!
#'
#' is.equidistant(c(1 : 10, 18))
#'
#' @export
#'
is.equidistant <- function(x) {

  if (!is.numeric(x)) stop("'x' needs to be numeric.")

  if ((xl <- length(x)) == 1) return(TRUE)

  xd <- diff(x)
  r <- all.equal(xd, rep(xd[1], xl - 1))

  if (is.logical(r)) return(TRUE) else return(FALSE)

}
