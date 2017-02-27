if ( !isGeneric("SSTindex") ) {
  setGeneric("SSTindex", function(index, ...)
    standardGeneric("SSTindex"))
}
#' Get Sea Surface Temperature Indices
#' 
#' @description 
#' Retrieve selected surface ocean indices provided by the Ocean Observations 
#' Panel for Climate (OOPC). If an active internet connection is available, the 
#' latest online version of the respective index is used instead of the 
#' built-in static data sets (end 27 February 2017).
#' 
#' @param index \code{character}, defaults to \code{"dmi"} (Dipole Mode Index). 
#' SST index under investigation, run \code{SSTindex()} for a full list of 
#' currently available options.
#' @param quiet \code{logical}, defaults to \code{FALSE}. Determines whether 
#' status messages and warnings are suppressed.
#' @param ... Additional arguments passed to \code{\link{download.file}} (except 
#' for 'destfile' and 'quiet'), ignored if 'index' is missing.
#' 
#' @return If 'index' is missing, a \code{list} of available indices divided 
#' by region of interest (Pacific, Atlantic, Indian), else a 2-column 
#' \code{data.frame} with SST index values per time step.
#' 
#' @seealso 
#' Ocean Observations Panel for Climate (2017) The State of the Ocean Climate. 
#' Available online: \url{http://stateoftheocean.osmc.noaa.gov/all/}.
#' 
#' @references 
#' Detsch F, Otte I, Appelhans T, Hemp A, Nauss T (2016) Seasonal and long-term 
#' vegetation dynamics from 1-km GIMMS-based NDVI time series at Mt. 
#' Kilimanjaro, Tanzania. \emph{Remote Sensing of Environment} \strong{178}, 
#' 70-83, \url{http://dx.doi.org/10.1016/j.rse.2016.03.007}.
#' 
#' Otte I, Detsch F, Mwangomo E, Hemp A, Appelhans T, Nauss T (2017) 
#' Multidecadal Trends and Interannual Variability of Rainfall as Observed from 
#' Five Lowland Stations at Mt. Kilimanjaro, Tanzania. \emph{Journal of 
#' Hydrometeorology} \strong{18}, 349-361, 
#' \url{http://dx.doi.org/10.1175/JHM-D-16-0062.1}.
#' 
#' @examples 
#' ## list available indices
#' SSTindex()
#' 
#' ## nino 3.4 region
#' oni <- SSTindex(index = "nino34", quiet = TRUE)
#' plot(oni[, 1], oni[, 2], type = "l", xlab = "Time", ylab = "ONI",
#'      main = expression(bold("Oceanic Ni" * tilde(n) * "o Index") ~ 
#'                        "(Ni" * tilde(n) * "o 3.4 region)"))
#' 
#' @export SSTindex
#' @name SSTindex
NULL

################################################################################
### function using 'character' input ###########################################
#' @aliases SSTindex,character-method
#' @rdname SSTindex
setMethod("SSTindex",
          signature(index = "character"),
          function(index = "dmi", quiet = FALSE, ...) {
  
  ## check validity of specified 'index'           
  index <- index[1]          
  if (!index %in% unlist(SSTindex()))
    stop("Specified index not found, run SSTindex() for available options.\n")
    
  ## try to download latest online version
  dn <- "http://stateoftheocean.osmc.noaa.gov/sur/data/"
  fl <- paste0(dn, index, ".nc")
  
  df <- tempfile(index, fileext = ".nc")
  dl <- try(utils::download.file(fl, df, quiet = quiet, ...), silent = TRUE)
  
  ## if download failed, use local version
  nc <- if (inherits(dl, "try-error")) {
    if (!quiet)
      warning("Latest online version of specified index could not be ",  
              "retrieved, using offline information.\n")
    
    SSTindices <- readRDS(system.file("extdata", "SSTindices.rds", 
                                      package = "ESD"))
    out <- SSTindices[, c("date", index)]                      
    
  ## else extract relevant information from downloaded netcdf file  
  } else {
    nc <- RNetCDF::open.nc(df)
    rd <- RNetCDF::read.nc(nc)
    ut <- RNetCDF::att.get.nc(nc, names(rd)[1], "units")
    dt <- RNetCDF::utcal.nc(ut, value = rd[[1]], type = "c")
    
    # ts(rd[[2]], frequency = 365.25 / 7, start = lubridate::decimal_date(dt[1]))
    out <- data.frame(dt, rd[[2]])
    names(out) <- c("date", index)
  }
  
  return(out)
})

################################################################################
### function using 'missing' input #############################################
#' @aliases SSTindex,missing-method
#' @rdname SSTindex
setMethod("SSTindex",
          signature(index = "missing"),
          function() {
  list("Pacific" = paste0("nino", c("12", "3", "34", "4")),
       "Atlantic" = c("tna", "tsa", "nat", "sat"), 
       "Indian" = c("wtio", "setio", "dmi", "swio"))
})