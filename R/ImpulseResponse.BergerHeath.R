#' Bioturbation impulse response
#'
#' Calculate the impulse response function of bioturbation for a given mixing
#' and pulse depth.
#'
#' This is an implementation of the solution from Berger and Heath (1968); as
#' given e.g. on page 65 in Officer and Lynch.
#'
#' @param z numeric vector of depths.
#' @param d mixing depth in the same units as \code{depth}.
#' @param z0 depth of the pulse without bioturbation in the same units as
#'   \code{depth}.
#' @return The weights of the bioturbation impulse response function.
#'
#' @references
#' Berger, W. and Heath, G. R.: Vertical Mixing in Pelagic Sediments,
#' J. Mar. Research 26, 135–143, 1968.
#'
#' Officer and Lynch.
#'
#' @author Thomas Laepple
#' @export
ImpulseResponse.BergerHeath <- function(z, d, z0 = 0) {

  x <- z0 + d - z
  epsilon <- x / d
  result <- 1 / d * exp(-epsilon)
  result[z > (z0 + d)] <- 0

  return(result)
}

