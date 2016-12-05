#' MODIS and GIMMS Download Chain
#'
#' @description
#' This is one of the core functions of the \strong{exploratories} package as it
#' invokes the product download of MODIS or GIMMS in preparation for
#' preprocessing and the application of the EOT algorithm.
#'
#' @param type \code{character}. Currently available options are "MODIS"
#' (default) and "GIMMS".
#' @param ... Additional arguments passed to the underlying download functions
#' for MODIS (\code{\link{runGdal}}; except for 'product' and 'collection', with
#' the latter set to \code{"006"}) and GIMMS (\code{\link{downloadGimms}};
#' except for 'version', which is set to \code{1L}) depending on 'type'.
#'
#' @return
#' A \code{character} vector of local filenames.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{runGdal}}, \code{\link{downloadGimms}}.
#'
#' @export download
#' @name download
download <- function(type = c("MODIS", "GIMMS"),
                     ...) {

  if (type[1] == "MODIS") {
    MODIS::runGdal(product = c("MOD13Q1", "MYD13Q1"), collection = "006", ...)
  } else if (type[1] == "GIMMS") {
    gimms::downloadGimms(version = 1L, ...)
  } else {
    stop("Specified product not supported.\n")
  }
}
