#' Evaluation Cycle of EOT-Based NDVI Resampling
#'
#' @description
#' Evaluate the performance of EOT-based spatial resampling based on selected
#' error and regression metrics, including mean error (ME), mean absolute error
#' (MAE), root-mean-square-error (RMSE) as well as correlation coefficient
#' (\eqn{r}) and coefficient of determination (\eqn{R^2}).
#'
#' @param pred \code{Raster*} object. Predictor data for EOT analysis.
#' @param resp \code{Raster*} object. Response data for EOT analysis.
#' @param training \code{integer}. Indices of training layers in 'pred' and
#' 'resp'. All non-included layers are automatically used for model testing.
#' @param n \code{integer}. Number of EOT modes to calculate.
#' @param var \code{numeric}. Minimum amount of variance to be explained. If
#' \code{NULL} (default), all calculated EOT modes are used for
#' \code{\link[remote]{predict}}-ion.
#' @param deseasoning \code{logical}. If \code{TRUE}, \code{\link{deseason}}-ing
#' is applied prior to the actual EOT procedure.
#' @param cores \code{integer}. Number of cores for parallel processing.
#' @param ... Additional arguments passed to \code{\link{eot}}.
#'
#' @return
#' A \code{list} of length 2. The content of the first (second) slot is a
#' \code{data.frame} (\code{RasterLayer}) with spatial (temporal) error and
#' regression metrics (RMSE).
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{evaluate}}, \code{\link{deseason}}, \code{\link{eot}}, 
#' \code{\link{nXplain}}. 
#'
#' @references
#' Detsch F, Otte I, Appelhans T, Hemp A, Nauss T (2016) Seasonal and long-term
#' vegetation dynamics from 1-km GIMMS-based NDVI time series at Mt.
#' Kilimanjaro, Tanzania. Remote Sensing of Environment 178, 70-83,
#' doi:10.1016/j.rse.2016.03.007.
#'
#' @export evaluateCycle
#' @name evaluateCycle
evaluateCycle <- function(pred,
                        resp,
                        training,
                        n = 10L,
                        var = NULL,
                        deseasoning = FALSE,
                        cores = 1L,
                        ...) {


  ### prerequisites -----

  ## apply deseasoning (optional)
  if (deseasoning) {
    pred <- remote::deseason(pred)
    resp <- remote::deseason(resp)
  }


  ### apply eot algorithm -----

  ## split 'pred' and 'resp' into training and test data
  test <- (1:raster::nlayers(pred))[-training]

  gimms_stck_pred <- raster::subset(pred, training)
  gimms_stck_eval <- raster::subset(pred, test)

  mod_stck_pred <- raster::subset(resp, training)
  mod_stck_eval <- raster::subset(resp, test)

  ## calculate EOT
  ndvi_modes <- remote::eot(x = gimms_stck_pred, y = mod_stck_pred, n = n, ...)

  ## get number of modes required to explain 'var' percent of variance
  if (!is.null(var))
    n <- remote::nXplain(ndvi_modes, var)

  ## perform spatial resampling based on test data
  mod_predicted <- remote::predict(object = ndvi_modes,
                                   newdata = gimms_stck_eval,
                                   n = n, cores = cores)


  ### calculate error and regression metrics -----

  ## extract values
  tmp_val_obs <- raster::getValues(mod_stck_eval)
  tmp_val_prd <- raster::getValues(mod_predicted)

  ## spatial metrics
  tmp_me <- colMeans(tmp_val_prd - tmp_val_obs, na.rm = TRUE)
  tmp_mae <- colMeans(abs(tmp_val_prd - tmp_val_obs), na.rm = TRUE)
  tmp_rmse <- sqrt(colMeans((tmp_val_prd - tmp_val_obs)^2, na.rm = TRUE))
  tmp_r <- diag(stats::cor(tmp_val_prd, tmp_val_obs, use = "complete.obs"))
  tmp_rsq <- tmp_r^2

  tmp_scores <- data.frame(ME = tmp_me, MAE = tmp_mae, RMSE = tmp_rmse,
                           R = tmp_r, Rsq = tmp_rsq)

  ## temporal rmse
  num_rmse <- sapply(1:nrow(tmp_val_obs), function(i) {
    num_obs <- tmp_val_obs[i, ]
    num_prd <- tmp_val_prd[i, ]
    sqrt(mean((num_prd-num_obs)^2, na.rm = TRUE))
  })

  rst_rmse <- raster::subset(mod_stck_eval, 1)
  rst_rmse <- raster::setValues(rst_rmse, num_rmse)

  ## return spatial and temporal metrics
  list(spatial = tmp_scores, temporal = rst_rmse)
}
