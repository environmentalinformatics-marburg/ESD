#' MODIS Preprocessing Chain
#'
#' @description
#' MODIS preprocessing chain including layer clipping, two-fold quality-control,
#' fortnightly composite creation, and subsequent combination of Terra and Aqua
#' imagery.
#'
#' @param dsn \code{character}. Target folder for file output.
# #' @param product \code{character}, see \code{\link{getProduct}}.
#' @param ext \code{Extent}, or any object from which an \code{Extent} can be
#' extracted, see \code{\link[raster]{crop}}.
#' @param cores \code{integer}. Number of cores for parallel processing.
#' @param ... Additional arguments passed to \code{\link{runGdal}}.
#'
#' @return
#' A preprocessed \code{Raster*} object.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{runGdal}}, \code{\link[raster]{crop}}.
#'
#' @export preprocessMODIS
#' @name preprocessMODIS
preprocessMODIS <- function(dsn = getwd(),
                              product = c("MOD13Q1", "MYD13Q1"),
                              ext = NULL,
                              cores = parallel::detectCores() - 1,
                              ...) {

  ### environmental stuff -----

  ## if not existent, create target folder
  if (!dir.exists(dsn))
    dir.create(dsn)

  ## parallelization
  cl <- parallel::makePSOCKcluster(cores)
  jnk <- parallel::clusterEvalQ(cl, {
    library(raster)
    library(MODIS)
    library(Rsenal)
  })


  ### preprocessing -----

  ## loop over products
  lst_prd <- lapply(product, function(h) {

    dir_prd <- paste0(dsn, "/", h, ".006")
    if (!dir.exists(dir_prd))
      dir.create(dir_prd)

    ## crop images
    parallel::clusterExport(cl, c("h", "dir_prd", "ext"), envir = environment())

    rst_crp <- parallel::parLapply(cl, c("NDVI", "pixel_reliability",
                                         "VI_Quality", "composite_day_of_the_year"),
                                   function(i) {

      # list and import available files
      fls <- list.files(paste0(getOption("MODIS_outDirPath"), "/", h, ".006"),
                        pattern = paste0(i, ".tif$"), full.names = TRUE)
      rst <- raster::stack(fls)

      # crop
      dir_out <- paste0(dir_prd, "/crp")
      if (!dir.exists(dir_out)) dir.create(dir_out)

      fls_out <- paste0(dir_out, "/", basename(fls))

      lst_out <- lapply(1:(raster::nlayers(rst)), function(j) {
        if (file.exists(fls_out[j])) {
          raster::raster(fls_out[j])
        } else {
          rst_out <- raster::crop(rst[[j]], ext, snap = "out")

          # apply scale factor
          if (i %in% c("NDVI", "EVI"))
            rst_out <- rst_out * 0.0001

          # save and return cropped layers
          raster::writeRaster(rst_out, filename = fls_out[j],
                              format = "GTiff", overwrite = TRUE)
        }
      })

      raster::stack(lst_out)
    })


    ### quality control, step #1: -----
    ### discard clouds, snow/ice and filled pixels using 'pixel_reliability'

    dir_qc1 <- paste0(dir_prd, "/qc1")
    if (!dir.exists(dir_qc1)) dir.create(dir_qc1)

    ## perform quality check #1 for both NDVI and EVI
    fls_qc1 <- paste0(dir_qc1, "/", names(rst_crp[[1]]), ".tif")

    parallel::clusterExport(cl, c("fls_qc1", "rst_crp"), envir = environment())
    lst_qc1 <- parallel::parLapply(cl, 1:nlayers(rst_crp[[1]]), function(i) {
                         if (file.exists(fls_qc1[i])) {
                           raster::raster(fls_qc1[i])
                         } else {
                           raster::overlay(rst_crp[[1]][[i]], rst_crp[[2]][[i]],
                                           fun = function(x, y) {
                                             x[!y[] %in% c(0, 1)] <- NA
                                             return(x)
                                           }, filename = fls_qc1[i],
                                           overwrite = TRUE, format = "GTiff")
                         }
                       })

    rst_qc1 <- stack(lst_qc1); rm(lst_qc1)


    ### quality control, step #2: -----
    ### discard cloudy pixels based on 'state_250m' flags

    dir_qc2 <- paste0(dir_prd, "/qc2")
    if (!dir.exists(dir_qc2)) dir.create(dir_qc2)

    ## perform quality check #2 for both NDVI and EVI
    fls_qc2 <- paste0(dir_qc2, "/", names(rst_qc1), ".tif")

    parallel::clusterExport(cl, c("fls_qc2", "rst_qc1"), envir = environment())

    lst_qc2 <- parallel::parLapply(cl, 1:(raster::nlayers(rst_qc1)), function(i) {
                         if (file.exists(fls_qc2[i])) {
                           raster::raster(fls_qc2[i])
                         } else {
                           raster::overlay(rst_qc1[[i]], rst_crp[[3]][[i]],
                                           fun = function(x, y) {
                             id <- sapply(y[], function(k) {
                               bin <- Rsenal::number2binary(k, 16, TRUE)
                               quality <- substr(bin, 15, 16)

                               if (quality == "00") {
                                 return(TRUE)
                               } else if (quality %in% c("10", "11")) {
                                 return(FALSE)
                               } else {
                                 useful <- !substr(bin, 11, 14) %in% c("1101", "1110")
                                 aerosol <- substr(bin, 9, 10) != "11"
                                 adjacent <- substr(bin, 8, 8) == "0"
                                 mixed <- substr(bin, 6, 6) == "0"
                                 snow <- substr(bin, 2, 2) == "0"
                                 shadow <- substr(bin, 1, 1) == "0"

                                 all(useful, aerosol, adjacent, mixed, snow, shadow)
                               }
                             })

                             x[!id] <- NA
                             return(x)
                           }, filename = fls_qc2[i], overwrite = TRUE, format = "GTiff")
                         }
                       })

    rst_qc2 <- raster::stack(lst_qc2); rm(lst_qc2)


    ### fortnightly aggregation -----

    rst_mvc <- MODIS::temporalComposite(rst_qc2, rst_crp[[4]], "fortnight",
                                        cores = cores)

    ## write to disk
    dir_mvc <- paste0(dir_prd, "/mvc")
    if (!dir.exists(dir_mvc)) dir.create(dir_mvc)

    dts_mvc <- names(rst_mvc)
    fls_mvc <- paste0(dir_mvc, "/", h, ".", dts_mvc, ".250m_16_days_NDVI.tif")

    parallel::clusterExport(cl, c("rst_mvc", "fls_mvc"), envir = environment())
    do.call(raster::stack,
            parallel::parLapply(cl, 1:raster::nlayers(rst_mvc), function(i) {
              if (file.exists(fls_mvc[i])) {
                raster::raster(fls_mvc[i])
              } else {
                raster::writeRaster(raster::subset(rst_mvc, i),
                                    filename = fls_mvc[i],
                                    format = "GTiff", overwrite = TRUE)
              }
            }))
  })


  ### combine terra and aqua-modis -----

  ## target folder and files
  dir_mcd <- paste0(dsn, "/MCD13Q1.006")
  if (!dir.exists(dir_mcd)) dir.create(dir_mcd)

  fls_mcd <- gsub("MOD13Q1", "MCD13Q1", names(lst_prd[[1]]))
  fls_mcd <- paste0(dir_mcd, "/", fls_mcd, ".tif")

  ## if two files are available, create maximum value composite, else take terra
  dts_terra <- MODIS::extractDate(lst_prd[[1]])$inputLayerDates
  dts_aqua <- MODIS::extractDate(lst_prd[[2]])$inputLayerDates

  parallel::clusterExport(cl, c("dts_terra", "dts_aqua", "lst_prd", "fls_mcd"),
                          envir = environment())
  rst_mcd <- do.call(raster::stack,
                     parallel::parLapply(cl, 1:length(fls_mcd), function(i) {
                       if (file.exists(fls_mcd[i])) {
                         raster::raster(fls_mcd[i])
                       } else {
                         if (!(dts_terra[i] %in% dts_aqua)) {
                           raster::writeRaster(lst_prd[[1]][[i]], filename = fls_mcd[i],
                                               format = "GTiff", overwrite = TRUE)
                         } else {
                           id <- which(dts_aqua == dts_terra[i])
                           raster::overlay(lst_prd[[1]][[i]], lst_prd[[2]][[id]],
                                           fun = function(...) max(..., na.rm = TRUE),
                                           filename = fls_mcd[i], format = "GTiff", overwrite = TRUE)
                         }
                       }
                     }))


  ### apply whittaker smoother -----

  ## target folder and files
  dir_wht <- paste0(dir_mcd, "/whittaker")
  if (!dir.exists(dir_wht)) dir.create(dir_wht)

  lst_wht <- MODIS::whittaker.raster(rst_mcd, outDirPath = dir_wht)
  rst_wht <- raster::stack(lst_wht)

  ## deregister parallel backend and return results
  parallel::stopCluster(cl)
  return(rst_wht)
}
