# ------------------------------------------------------------------------------
# Marine d18O calibration functions from Anand et al. (2003)
# ------------------------------------------------------------------------------

#' Calculate seawater d18O
#'
#' Calculate the oxygen isotope composition (d18O) of seawater from its
#' salinity.
#'
#' This is an implementation of the equation from Duplessy et al. (1991) based
#' on the salinity for water samples collected in the upper 250 m of the
#' Atlantic Ocean during the GEOSECS expedition, as reported in Anand et
#' al. 2003.
#'
#' @param Salinity salinity in grams per litre or parts per thousand.
#' @return The oxygen isotope composition of seawater (d18Ow) as permille
#'   deviation from the SMOW scale.
#'
#' @references
#' Anand, P., Elderfield, H. and Conte, M. H.: Calibration of Mg/Ca thermometry
#' in planktonic foraminifera from a sediment trap time series,
#' Paleoceanography, 18(2), 1050, doi:10.1029/2002PA000846, 2003.
#'
#' Duplessy, J.-C., L. Labeyrie, A. Juillet-Leclerc, F. Maitre, J. Dupart, and
#' M. Sarnthein, Surface salinity reconstruction of the north Atlantic Ocean
#' during the last glacial maximum, Oceanol. Acta, 14, 311– 324, 1991.
#'
#' @author Andrew Dolman
#' @examples
#'   d18OwFromSalinity(30)
#' @seealso \code{\link{d18OcFromd18OwTemp}}, \code{\link{CalcifTemp}}
#' @export
d18OwFromSalinity <- function(Salinity) {
  -19.264 + 0.558 * Salinity
}

#' Paleotemperature Equation
#'
#' Calculate the equilibrium oxygen isotope composition (d18O) of calcification
#' from seawater oxygen isotope composition (d18Ow) and temperature
#' (Paleotemperature Equation).
#'
#' This is an implementation of the paleotemperature equation of O’Neil et
#' al. (1969) and Shackleton (1974). The factor 0.27 was used to convert from
#' water on the SMOW scale to calcite on the Peedee Belemnite (PDB) scale, as
#' described in Anand et al. (2003).
#'
#' @param d18Ow oxygen isotope composition of seawater in permille.
#' @param Temp temperature in degree celcius.
#' @return the oxygen isotope composition of calcite on the PDB scale.
#'
#' @references
#' Anand, P., Elderfield, H. and Conte, M. H.: Calibration of Mg/Ca thermometry
#' in planktonic foraminifera from a sediment trap time series,
#' Paleoceanography, 18(2), 1050, doi:10.1029/2002PA000846, 2003.
#'
#' O’Neil, J. R., R. N. Clayton, and T. K. Mayeda, Oxygen isotope fractionation
#' in divalent metal carbonates, 51, 5547– 5558, 1969.
#'
#' Shackleton, N. J., Attainment of isotopic equilibrium between ocean water and
#' the benthonic foraminiferal genus Uvigerina: Isotopic changes in the ocean
#' during the last glacial, Cent. Natl. Rech. Sci. Colloq. Int., 219, 203–209,
#' 1974.
#'
#' @author Andrew Dolman
#' @examples
#'   d18OcFromd18OwTemp(d18Ow = 0.6, 1:26)
#' @seealso \code{\link{d18OwFromSalinity}}, \code{\link{CalcifTemp}}
#' @export
d18OcFromd18OwTemp <- function(d18Ow, Temp) {
  (d18Ow - 0.27) + (4.38 - (4.38^2 - 4 * 0.1 * (16.9 - Temp))^0.5) / (2 * 0.1)
}

#' Inverse Paleotemperature Equation
#'
#' Calculate the calcification temperature from the oxygen isotope
#' compositions of the calcite (d18O) and of the seawater (d18Ow) (inverse of
#' the Paleotemperature Equation).
#'
#' @param d18Oc d18O of calcite in permille on the PDB scale.
#' @param d18Ow d18O of seawater in permille on the SMOW scale.
#' @return The calcification temperature in degree Celcius.
#'
#' @author Andrew Dolman
#' @examples
#'   CalcifTemp(d18OcFromd18OwTemp(d18Ow = 0.6, 1:26), 0.6)
#' @export
CalcifTemp <- function(d18Oc, d18Ow) {
  16.9 - 4.38 * (d18Oc - (d18Ow - 0.27)) + 0.1 * (d18Oc - (d18Ow - 0.27))^2
}

# ------------------------------------------------------------------------------
# Radiocarbon age calibration functions
# ------------------------------------------------------------------------------

#' Calibrate 14C age with Bchron
#'
#' Calibrate a set of 14C ages in a dataframe using the BChron package.
#'
#' This function is a wrapper for \code{\link[Bchron]{BchronCalibrate}}.
#'
#' @param df dataframe of two columns giving the 14C ages and their standard error.
#' @param age.14C character; name of the column in \code{df} with the 14C ages;
#'   defaults to \code{"age.14C"}.
#' @param age.14C.se character; name of the column in \code{df} with 1 standard
#'   error 14C age uncertainty; defaults to \code{"age.14C.se"}.
#' @param curve character; name pf the calibration curve to use; defaults to \code{"intcal20"}.
#' @param return.type character; signal the amount of returned information:
#'   return only the ammended dataframe (\code{"df"}) or additionally the list
#'   of PDFs (\code{"lst"}); defaults to (\code{"df"}).
#' @param offset character; name of an optional additional column in \code{df}
#'   supplying a variable offset applied to all 14C ages; the default
#'   \code{NULL} means to apply no offset.
#' @return A dataframe or list.
#' @author Andrew Dolman
#' @examples
#' # With defaults
#' dat <- data.frame(age.14C = c(2000, 20000),
#'                   age.14C.se = c(100, 200))
#' CalibrateAge(dat)
#'
#' # Change the calibration
#' CalibrateAge(dat, curve = "marine13")
#'
#' # Return the PDFs
#' cal.lst <- CalibrateAge(dat, curve = "marine13", return = "lst")
#' with(cal.lst[[2]][[1]][[1]], {plot(ageGrid, densities)})
#'
#' # Use different column names
#' dat <- data.frame(radiocarbon.age = c(2000, 20000),
#'                  se = c(100, 200))
#' CalibrateAge(dat, age.14C = "radiocarbon.age", age.14C.se = "se")
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[Bchron]{BchronCalibrate}}
#'  \code{\link[stattools]{SummariseEmpiricalPDF}}
#' @export
CalibrateAge <- function(df, age.14C = "age.14C",
                         age.14C.se = "age.14C.se",
                         curve = "intcal20",
                         return.type = "df", offset = NULL) {

  return.type <- match.arg(return.type, choices = c("df", "lst"))
  curve <- match.arg(curve, choices = c("intcal13", "shcal13", "marine13",
                                        "intcal20", "marine20", "shcal20",
                                        "normal"))

  if (is.null(offset)) {
    df$offset <- 0
  } else {
    df$offset <- df[[offset]]
  }

  cal.ages <- lapply(1 : nrow(df), function(x) {
    tryCatch(Bchron::BchronCalibrate(
      ages = df[[age.14C]][x] + df[["offset"]][x],
      ageSds = df[[age.14C.se]][x],
      calCurves = curve,
      ids = x),
      error = function(i){
        cat(strsplit(as.character(i), " : ", fixed = TRUE)[[1]][2])
        NA
      })
  })


  # Use mean and sd of empirical PDFs as point estimates of calendar ages
  df$age.14C.cal <- sapply(cal.ages, function(x) {
    if (is.na(x) == FALSE) {
      stattools::SummariseEmpiricalPDF(x[[1]]$ageGrid, x[[1]]$densities)["median"]
    } else {
      NA
    }
  })

  df$age.14C.cal.se <- sapply(cal.ages, function(x){
    if (is.na(x) == FALSE) {
      stattools::SummariseEmpiricalPDF(x[[1]]$ageGrid, x[[1]]$densities)["sd"]
    } else {
      NA
    }
  })

  if (return.type == "df") {
    out <- df
  }

  if (return.type == "lst") {
    out <- list(df = df, cal.ages = cal.ages)
  }

  return(out)
}
