#' GIMMS NDVI3g.v1 Processing Chain
#'
#' @description
#' This function covers the major tasks related to GIMMS NDVI3g.v1 preprocessing
#' including file download (\code{\link{downloadGimms}}), rasterization and
#' quality control (\code{\link{rasterizeGimms}}), creation of monthly value
#' composites (\code{\link{monthlyComposite}}), and subsequent application of
#' the Whittaker smoother (\code{\link{whittaker.raster}}).
#'
#' @param x \code{character}. Vector of local filepaths created e.g. from
#' \code{\link{downloadGimms}}. See also \code{\link{rasterizeGimms}}.
#' @param ext \code{Extent}, or any object from which an \code{Extent} can be
#' extracted, see \code{\link[raster]{crop}}.
#' @param keep \code{integer}. Flag values of NDVI3g.v1 pixels to spare during
#' quality control, see \code{\link{rasterizeGimms}}.
# #' @param fun \code{function}. Used to calculate monthly composite layers,
# #' defaults to \code{\link{max}}, see \code{\link{monthlyComposite}}.
#' @param cores \code{integer}. Number of cores for parallel computing.
#' @param ... Additional arguments passed to \code{\link{whittaker.raster}}
#' (except for 'vi' and 'timeInfo').
#'
#' @return
#' A \code{RasterStack} of Whittaker-smoothed GIMMS NDVI3g.v1 monthly value
#' composites.
#'
#' @seealso
#' \code{\link{downloadGimms}}, \code{\link{rasterizeGimms}},
#' \code{\link{monthlyComposite}}, \code{\link{whittaker.raster}}.
#'
#' @examples
#' \dontrun{
#' ## modis options
#' MODIS::MODISoptions(localArcPath = "MODIS/MODIS_ARC",
#'                     outDirPath = "MODIS/MODIS_ARC/PROCESSED/",
#'                     outProj = "+init=epsg:4326",
#'                     MODISserverOrder = c("LAADS", "LPDAAC"))
#'
#' ## clipping extent
#' ext <- raster::getData(country = "DEU", level = 0)
#'
#' ## initialize processing
#' rst <- preprocessGIMMS(ext = ext,
#'                          keep = 0)  # keep pixels flagged 'good' only
#'
#' ## display the first 12 months (Jul 1981 to Jun 1982)
#' raster::plot(rst[[1:12]])
#' }
#'
#' @export preprocessGIMMS
#' @name preprocessGIMMS
preprocessGIMMS <- function(x,
                            ext = NULL,
                            keep = NULL,
                            # fun = max,
                            cores = 1L,
                            ...) {

  ## rasterize and apply quality control
  rst <- gimms::rasterizeGimms(x, ext, keep = keep, cores = cores)

  # ## create monthly composites
  # mvc <- gimms::monthlyComposite(rst, indices = gimms::monthlyIndices(x),
  #                                fun = fun, cores = cores, na.rm = TRUE)

  ## apply whittaker smoother
  dts <- gimms::monthlyIndices(x, timestamp = TRUE)
  # dts <- seq(dts[1], dts[length(dts)], "month")
  nfo <- MODIS::orgTime(dts)

  wht <- MODIS::whittaker.raster(rst, timeInfo = nfo, ...)

  ## discard values larger than 1
  cl <- parallel::makePSOCKcluster(cores)
  jnk <- parallel::clusterEvalQ(cl, library(raster))

  wht <- parallel::parLapply(cl, wht, function(i) {
    i[i[] > 1] <- NA
    return(i)
  })

  parallel::stopCluster(cl)

  ## return stacked images
  raster::stack(wht)
}
