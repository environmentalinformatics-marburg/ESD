#' GIMMS NDVI3g.v1 Processing Chain
#'
#' @description
#' This function covers the major tasks related to GIMMS NDVI3g.v1 preprocessing
#' including file download (\code{\link{downloadGimms}}), rasterization and
#' quality control (\code{\link{rasterizeGimms}}), creation of monthly value
#' composites (\code{\link{monthlyComposite}}), and subsequent application of
#' the Whittaker smoother (\code{\link{whittaker.raster}}).
#'
#' @param dsn \code{character}, defaults to current working directory. Target
#' folder for file download passed to \code{\link{downloadGimms}}.
#' @param ext \code{Extent}, or any object from which an \code{Extent} can be
#' extracted, see \code{\link[raster]{crop}}.
#' @param keep \code{integer}. Flag values of NDVI3g.v1 pixels to spare during
#' quality control, see \code{\link{rasterizeGimms}}.
# #' @param fun \code{function}. Used to calculate monthly composite layers,
# #' defaults to \code{\link{max}}, see \code{\link{monthlyComposite}}.
#' @param lambda \code{character} or \code{integer}. Yearly lambda value passed
#' to \code{\link{whittaker.raster}}.
#' @param cores \code{integer}. Number of cores for parallel computing.
#' @param outDirPath \code{character}, defaults to current working directory.
#' Target folder for Whittaker-smoothed files passed to
#' \code{\link{whittaker.raster}}.
#' @param ... Further arguments passed to \code{\link{downloadGimms}} except for
#' 'version' (is set to \code{1L}) and 'dsn' (see above).
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
preprocessGIMMS <- function(dsn = getwd(),
                              ext = NULL,
                              keep = NULL,
                              # fun = max,
                              lambda = 5000,
                              cores = 1L,
                              outDirPath = getwd(),
                              ...) {

  ## rasterize and apply quality control
  rst <- gimms::rasterizeGimms(fls, ext, keep = keep, cores = cores)

  # ## create monthly composites
  # mvc <- gimms::monthlyComposite(rst, indices = gimms::monthlyIndices(fls),
  #                                fun = fun, cores = cores, na.rm = TRUE)

  ## apply whittaker smoother
  dts <- gimms::monthlyIndices(fls, timestamp = TRUE)
  # dts <- seq(dts[1], dts[length(dts)], "month")
  nfo <- MODIS::orgTime(dts)

  if (!dir.exists(outDirPath))
    dir.create(outDirPath)

  wht <- MODIS::whittaker.raster(rst, outDirPath = outDirPath, timeInfo = nfo,
                                 lambda = lambda, format = "GTiff",
                                 overwrite = TRUE)

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
