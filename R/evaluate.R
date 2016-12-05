#' Visualize EOT Validation Results
#'
#' @description
#' Evaluate performance of EOT-based spatial resampling \emph{n} times and
#' subsequently visualize derived error and regression metrics.
#'
#' @param pred \code{Raster*} object. Predictor data for EOT analysis.
#' @param resp \code{Raster*} object. Response data for EOT analysis.
#' @param by \code{integer}. Increment of the sequence passed to
#' \code{\link{seq}}.
#' @param times \code{integer}. Determines the number of
#' \code{\link{evaluateEOT}} runs.
#' @param size \code{integer}. Sample size passed to \code{\link{sample}}.
#' @param ... Additional arguments passed to \code{\link{write.csv}}.
#'
#' @return
#' A \code{data.frame}.
#'
#' @author
#' Florian Detsch
#'
#' @export evaluate
#' @name evaluate
evaluate <- function(pred, resp, by = 24L, times = 10L, size = 5L, ...) {

  ## evaluate eot-based spatial resampling 'times' times
  lst_scores <- lapply(1:times, function(h) {

    # status message
    cat("Iteration #", h, "...\n", sep = "")

    # indices of training dataset
    indices <- do.call("c", lapply(1:by, function(i) {
      month_id <- seq(i, raster::nlayers(pred), by)

      set.seed(i + h)
      sort(sample(month_id, size))
    }))

    exploratories::evaluateEOT(pred, resp, training = indices)
  })

  lst_spatial <- lapply(lst_scores, "[[", 1)
  dat_spatial <- do.call("rbind", lst_spatial)

  ME.se <- plotrix::std.error(dat_spatial$ME, na.rm = TRUE)
  MAE.se <- plotrix::std.error(dat_spatial$MAE, na.rm = TRUE)
  RMSE.se <- plotrix::std.error(dat_spatial$RMSE, na.rm = TRUE)

  dat_spatial <- colMeans(dat_spatial, na.rm = TRUE)
  dat_spatial <- data.frame(ME = dat_spatial[1], ME.se, MAE = dat_spatial[2], MAE.se,
                            RMSE = dat_spatial[3], RMSE.se, Rsq = dat_spatial[5])

  write.csv(dat_spatial, ...)

  return(dat_spatial)
}
