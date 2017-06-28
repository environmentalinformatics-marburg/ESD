if ( !isGeneric("preprocessMODIS") ) {
  setGeneric("preprocessMODIS", function(x, ...)
    standardGeneric("preprocessMODIS"))
}
#' MODIS Preprocessing Chain
#'
#' @description
#' MODIS preprocessing chain including optional layer clipping, two-fold 
#' quality-control, fortnightly composite creation, and subsequent combination 
#' of Terra and Aqua imagery into half-monthly maximum value composites .
#'
#' @param x A named \code{list} of local MODIS SDS layers per product as 
#' returned by \code{\link{getHdf}}.
#' @param vi \code{character}. Vegetation index under consideration. Currently 
#' available options are "NDVI" (default) and "EVI".
#' @param dsn \code{character}. Target folder for file output.
# #' @param product \code{character}, defaults to the Terra (\code{"MOD13Q1"}) and
# #' Aqua-MODIS (\code{"MYD13Q1"}) 250-m NDVI products. See also
# #' \code{\link{getProduct}}.
#' @param ext \code{Extent}, or any object from which an \code{Extent} can be
#' extracted, see \code{\link[raster]{crop}}. If missing, layer clipping is 
#' skipped.
#' @param interval See \code{\link{temporalComposite}}.
#' @param whit \code{logical}. \code{TRUE} (default) invokes gap-filling via 
#' modified Whittaker smoothing, see \code{\link{whittaker.raster}}.
#' @param cores \code{integer}. Number of cores for parallel processing.
#' @param ... Additional arguments passed to \code{\link{whittaker.raster}}
#' (except for 'vi' and 'outDirPath').
# #' @param begin,end,tileH,tileV,buffer,SDSstring,job,wait,forceDownload,overwrite
# #' See \code{\link{runGdal}}.
#'
#' @return
#' A preprocessed \code{Raster*} object.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link[raster]{crop}}, \code{\link{whittaker.raster}}.
#'
#' @export preprocessMODIS
#' @name preprocessMODIS

# ################################################################################
# ### function using no input (i.e., call runGdal) ###############################
# #' @aliases preprocessMODIS,missing-method
# #' @rdname preprocessMODIS
# setMethod("preprocessMODIS",
#           signature(x = "missing"),
#           function(product = "M*D13Q1", collection = NULL,
#                    begin = NULL, end = NULL, 
#                    ext = NULL, tileH = NULL, tileV = NULL, buffer = 0, 
#                    SDSstring = NULL, job = NULL, wait = 0.5, 
#                    forceDownload = TRUE, overwrite = FALSE, 
#                    dsn = getwd(),
#                    cores = 1L,
#                    ...) {
# 
#   ### preprocessing -----
#             
#   ## (download and) extract required sds          
#   x <- MODIS::runGdal(product, collection, begin, end, ext, tileH, tileV, 
#                       buffer, SDSstring, job, checkIntegrity = TRUE, wait, 
#                       forceDownload, overwrite)
#   
#   ## call 'list' method
#   preprocessMODIS(x, dsn, ext, cores, ...)
# })


################################################################################
### function using 'list' input from runGdal ###################################
#' @aliases preprocessMODIS,list-method
#' @rdname preprocessMODIS
setMethod("preprocessMODIS",
          signature(x = "list"),
          function(x, 
                   vi = c("NDVI", "EVI"),
                   dsn = getwd(),
                   ext = NULL,
                   interval = "fortnight",
                   whit = TRUE,
                   cores = 1L,
                   ...) {
            
  ### environmental stuff -----
            
  ## if not existent, create target folder
  if (!dir.exists(dsn))
    dir.create(dsn)
            
  ## parallelization
  cl <- parallel::makePSOCKcluster(cores)
  on.exit(parallel::stopCluster(cl))
  
  jnk <- parallel::clusterEvalQ(cl, {
    library(raster)
    library(MODIS)
    library(Rsenal)
  })
  
  
  ### preprocessing -----
  
  ## loop over products
  product <- names(x)
  if (is.null(product)) 
    stop("'x' must be a named list as returned e.g. by getHdf().\n")
  
  lst_prd <- lapply(product, function(h) {
    
    dir_prd <- paste0(dsn, "/", h)
    if (!dir.exists(dir_prd))
      dir.create(dir_prd)
    
    
    ### clipping (optional) -----
    
    parallel::clusterExport(cl, c("x", "h", "dir_prd", "ext"), 
                            envir = environment())
    
    rst_crp <- parallel::parLapply(cl, c(vi[1], "pixel_reliability",
                                         "VI_Quality", "composite_day_of_the_year"),
                                   function(i) {
                                     
      # list and import available files
      fls <- sapply(x[[which(names(x) == h)]], function(j) j[grep(i, j)])
      rst <- raster::stack(fls)
                                     
      # crop
      if (!is.null(ext)) {
        dir_out <- paste0(dir_prd, "/crp")
        if (!dir.exists(dir_out)) dir.create(dir_out)
        
        fls_out <- paste0(dir_out, "/", basename(fls))
        
        lst_out <- lapply(1:(raster::nlayers(rst)), function(j) {
          if (file.exists(fls_out[j])) {
            raster::raster(fls_out[j])
          } else {
            tmp <- raster::crop(rst[[j]], ext, snap = "out") 
            
            raster::writeRaster(tmp, filename = fls_out[j],
                                format = "GTiff", overwrite = TRUE)
          }
        })
        
        rst <- raster::stack(lst_out); rm(lst_out)
        
      } else {
        lst <- lapply(1:3, function(k) {
          sapply(strsplit(basename(fls), "\\."), "[[", k)
        })
        
        prd <- lst[[1]]; dts <- lst[[2]]; sds <- lst[[3]]
        names(rst) <- paste(prd, dts, sds, sep = ".")
      }
      return(rst)        
    })
    
    
    ### scaling -----
    
    ## apply scale factor
    dir_scl <- paste0(dir_prd, "/scl")
    if (!dir.exists(dir_scl)) dir.create(dir_scl)
    fls_raw <- sapply(x[[which(names(x) == h)]], "[[", 1)
    fls_scl <- paste0(dir_scl, "/", basename(fls_raw))

    parallel::clusterExport(cl, c("rst_crp", "fls_scl"), envir = environment())
    
    lst_scl <- parallel::parLapply(cl, 1:raster::nlayers(rst_crp[[1]]), 
                                   function(i) {
      if (file.exists(fls_scl[i])) {
        raster::raster(fls_scl[i])
      } else {
        raster::calc(rst_crp[[1]][[i]], fun = function(x) x * 0.0001, 
                     filename = fls_scl[i], overwrite = TRUE)
      }
    })
    
    rst_scl <- raster::stack(lst_scl); rm(lst_scl)
    
    
    ### quality control, step #1: -----
    ### discard clouds, snow/ice and filled pixels using 'pixel_reliability'
    
    dir_qc1 <- paste0(dir_prd, "/qc1")
    if (!dir.exists(dir_qc1)) dir.create(dir_qc1)
    
    ## perform quality check #1 for both NDVI and EVI
    fls_qc1 <- paste0(dir_qc1, "/", names(rst_scl), ".tif")
    
    parallel::clusterExport(cl, c("fls_qc1", "rst_scl"), envir = environment())
    lst_qc1 <- parallel::parLapply(cl, 1:nlayers(rst_scl), function(i) {
      if (file.exists(fls_qc1[i])) {
        raster::raster(fls_qc1[i])
      } else {
        raster::overlay(rst_scl[[i]], rst_crp[[2]][[i]],
                        fun = function(x, y) {
                          x[!y[] %in% c(0, 1)] <- NA
                          return(x)
                        }, filename = fls_qc1[i], overwrite = TRUE)
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
    
    rst_mvc <- MODIS::temporalComposite(rst_qc2, rst_crp[[4]], 
                                        interval = interval, cores = cores)
    
    ## write to disk
    dir_mvc <- paste0(dir_prd, "/mvc")
    if (!dir.exists(dir_mvc)) dir.create(dir_mvc)
    
    dts_mvc <- names(rst_mvc)
    fls_mvc <- paste0(dir_mvc, "/", strsplit(h, "\\.")[[1]][1], ".", dts_mvc, 
                      ".250m_16_days_NDVI.tif")
    
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
  sns <- substr(product[1], 1, 3)
  dir_mcd <- paste0(dsn, "/", gsub(sns, "MCD", product[1]))
  if (!dir.exists(dir_mcd)) dir.create(dir_mcd)
  
  fls_mcd <- gsub(paste0("^", sns), "MCD", names(lst_prd[[1]]))
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
  
  if (whit) {
    ## target folder and files
    dir_wht <- paste0(dir_mcd, "/whittaker")
    if (!dir.exists(dir_wht)) dir.create(dir_wht)
    
    lst_wht <- MODIS::whittaker.raster(rst_mcd, outDirPath = dir_wht, ...)
    rst_wht <- raster::stack(lst_wht)
    return(rst_wht)
    
  } else 
    return(rst_mcd)
})
