#' Vegetation Analysis in the Biodiversity Exploratories
#'
#' We provide a set of functions to analyze long-term vegetation dynamics in the
#' Biodiversity Exploratories.
#'
#' @name exploratories-package
#' @aliases exploratoriespackage
#' @docType package
#' @title Vegetation Analysis in the Biodiversity Exploratories
#' @author Florian Detsch, Thomas Nauss
#'
#' @import gimms lattice latticeExtra methods MODIS parallel raster remote
#' @importFrom plotrix std.error
#' @importFrom Rsenal number2binary
#' @importFrom stats cor
#' @importFrom utils write.csv
#'
#' @keywords package
#'
NULL
#'
#' @docType data
#' @name albMODIS
#' @title Schwaebische Alb MODIS NDVI V006
#' @description Schwaebische Alb half-monthly MODIS NDVI (V006; 2012 to 2015).
#' @details This dataset contains half-monthly MODIS NDVI observations for parts
#' of the Schwaebische Alb, south-western Germany (Jan 2012 to Dec 2015). For
#' both the 16-day Terra (MOD13Q1.006) and Aqua-MODIS composite layers
#' (MYD13Q1.006), preprocessing included
#'
#' \itemize{
#' \item{the conduction of a comprehensive quality control based on the
#' 'pixel_reliability' and 'VI_quality' SDS and}
#' \item{the subsequent creation of half-monthly composites based on the
#' 'composite_day_of_the_year' SDS that line up exactly with the release
#' interval of GIMMS NDVI3g.}
#'
#' This was followed by the combination of the sensor-specific layers into
#' half-monthly maximum value composites and, finally, the application of a
#' modified Whittaker smoother to account for non-captured clouds and fill newly
#' introduced data gaps.
#'
#' }
#' @format \code{raster::RasterBrick}
#' @references
#' Didan K, Barreto Munoz A, Solano R, Huete A (2015) MODIS Vegetation Index
#' User's Guide (MOD13 Series). Version 3.00, June 2015 (Collection 6).
#' Available online: \url{https://lpdaac.usgs.gov/sites/default/files/public/product_documentation/mod13_user_guide.pdf}.
NULL
#'
#' @docType data
#' @name albGIMMS
#' @title Schwaebische Alb GIMMS NDVI3g.v1
#' @description Schwaebische Alb half-monthly GIMMS NDVI3g.v1 (2012 to 2015).
#' @details This dataset contains half-monthly GIMMS NDVI3g.v1 observations for
#' parts of the Schwaebische Alb, south-western Germany (Jan 2012 to Dec 2015).
#' Preprocessing included
#'
#' \itemize{
#' \item{the conduction of a comprehensive quality control based on the
#' accompanying quality flags followed by}
#' \item{the application of a modified Whittaker smoother to account for
#' non-captured clouds and fill newly introduced data gaps.}
#' }
#' @format \code{raster::RasterBrick}
#' @references
#' Pinzon JE, Tucker CJ (2014) A Non-Stationary 1981-2012 AVHRR NDVI3g Time
#' Series. Remote Sensing, 6(8), 6929-6960. Available online:
#' \url{http://www.mdpi.com/2072-4292/6/8/6929/htm}.
NULL
