#' MODIS and GIMMS Preprocessing Chain
#'
#' @description
#' This is one of the core functions of the \strong{exploratories} package as it
#' invokes the preprocessing chains of MODIS or GIMMS in preparation for the
#' application of the EOT algorithm.
#'
#' @param type \code{character}. Currently available options are "MODIS"
#' (default) and "GIMMS".
#' @param ... Additional arguments passed to the underlying preprocessing
#' functions for MODIS (\code{\link{processChainMODIS}}) and GIMMS
#' \code{\link{processChainGIMMS}} depending on 'type'.
#'
#' @return
#' A preprocessed \code{Raster*} object.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{processChainMODIS}}, \code{\link{processChainGIMMS}}
#'
#' @export processChain
#' @name processChain
processChain <- function(type = c("MODIS", "GIMMS"),
                         ...) {

  if (type[1] == "MODIS") {
    processChainMODIS(...)
  } else if (type[1] == "GIMMS") {
    processChainGIMMS(...)
  } else {
    stop("Specified product not supported.\n")
  }
}


