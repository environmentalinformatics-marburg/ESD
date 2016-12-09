#' Perform EOT-Based Spatial Downscaling
#' 
#' @description 
#' Perform EOT-based spatial downscaling based on user-defined predictor and 
#' response domains.
#' 
#' @param pred,resp \code{Raster*} objects used as predictor and response 
#' domains during EOT analysis.
#' @param neot \code{integer}. Number of EOT modes to calculate.
#' @param newdata \code{Raster*} object passed to \code{\link[remote]{predict}}.
#' @param nprd \code{integer}. Number of EOT modes used for prediction.
#' @param var \code{numeric}. Minimum amount of variance to be explained by the 
#' EOT modes. If specified, 'nprd' is adjusted automatically.
#' @param filename,format,overwrite Arguments passed to 
#' \code{\link[remote]{predict}} and its call to \code{\link{writeRaster}}. 
#' @param ... Additional arguments passed to \code{\link{eot}}.
#' 
#' @return 
#' A \code{Raster*} object.
#' 
#' @author 
#' Florian Detsch
#' 
#' @references 
#' Appelhans T, Detsch F, Nauss T (2015) \strong{remote}: Empirical Orthogonal 
#' Teleconnections in R. \emph{Journal of Statistical Software} 65(10), 1-19, 
#' doi:10.18637/jss.v065.i10. Available online: 
#' \url{https://www.jstatsoft.org/article/view/v065i10}.
#' 
#' @examples 
#' ## raw data
#' plot(albGIMMS[[49:52]])
#' 
#' ## downscaled data
#' prd <- downscale(pred = albGIMMS[[1:48]], resp = albMODIS[[1:48]], neot = 10L,
#'                  newdata = albGIMMS[[49:96]], var = 0.85)
#' plot(prd[[1:4]])                  
#' 
#' @export downscale
#' @name downscale
downscale <- function(pred, resp, neot = 1L, newdata, nprd = 1L, var = NULL, 
                      filename = "", format = "GTiff", overwrite = TRUE, ...) {
  
  ## calculate eot
  mod <- remote::eot(pred, resp, n = neot, ...)
  
  ## if specified, reduce number of utilized eot modes to minimum amount of 
  ## variance to be explained
  if (!is.null(var))
    nprd <- remote::nXplain(mod, var)
    
  ## predict newdata
  remote::predict(mod, newdata, nprd, 
                  filename = filename, format = format, overwrite = overwrite)
}