#' MODIS and GIMMS Preprocessing Chain
#'
#' @description
#' This is one of the core functions of the \strong{ESD} package as it invokes
#' the preprocessing chains of MODIS or GIMMS in preparation for the application
#' of the EOT algorithm.
#'
#' @param type \code{character}. Currently available options are "MODIS"
#' (default) and "GIMMS".
#' @param ... Additional arguments passed to the underlying preprocessing
#' functions for MODIS (\code{\link{preprocessMODIS}}) and GIMMS
#' \code{\link{preprocessGIMMS}} depending on 'type'.
#'
#' @return
#' A preprocessed \code{Raster*} object.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{preprocessMODIS}}, \code{\link{preprocessGIMMS}}
#'
#' @export preprocess
#' @name preprocess
preprocess <- function(type = c("MODIS", "GIMMS"),
                         ...) {

  if (type[1] == "MODIS") {
    preprocessMODIS(...)
  } else if (type[1] == "GIMMS") {
    preprocessGIMMS(...)
  } else {
    stop("Specified product not supported.\n")
  }
}


