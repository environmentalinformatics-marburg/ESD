#' Download of NDVI Products
#'
#' @description
#' This is one of the core functions of the \strong{ESD} package as it invokes
#' the download of selected NDVI products in preparation for preprocessing and
#' the application of the EOT algorithm.
#'
#' @param type \code{character}. Currently available options are "MODIS"
#' (default) and "GIMMS".
#' @param ... Additional arguments passed to the underlying download functions
#' for MODIS (\code{\link{runGdal}}; except for 'product', 'collection' (set to
#' \code{"006"}), and 'SDSstring' (set to \code{"101000000011"})) and GIMMS
#' (\code{\link{downloadGimms}}; except for 'version' (set to \code{1L}))
#' depending on 'type'.
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
#' @examples
#' \dontrun{
#' ## Reference extent (Schwaebische Alb)
#' ext <- system.file("extdata/alb.rds", package = "ESD")
#' ext <- readRDS(ext)
#'
#' ## MODIS download
#' mds <- download(extent = ext, begin = "2012001", end = "2015365",
#'                 job = "MCD13Q1.006_alb")
#'
#' ## GIMMS download
#' gms <- download("GIMMS", x = as.Date("2012-01-01"), y = as.Date("2015-12-31"),
#'                 dsn = "NDVI3g.v1_alb")
#' }
#'
#' @export download
#' @name download
download <- function(type = c("MODIS", "GIMMS"),
                     ...) {

  if (type[1] == "MODIS") {
    MODIS::runGdal(product = c("MOD13Q1", "MYD13Q1"), collection = "006",
                   SDSstring = "101000000011", ...)
  } else if (type[1] == "GIMMS") {
    gimms::downloadGimms(version = 1L, ...)
  } else {
    stop("Specified product not supported.\n")
  }
}
