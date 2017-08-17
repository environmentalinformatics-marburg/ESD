#' Evaluate EOT-Based NDVI Resampling
#'
#' @description
#' Evaluate the performance of EOT-based spatial resampling (of the NDVI) on the
#' basis of selected error and regression metrics. The evaluation procedure is
#' carried out \emph{n} times, thus ensuring the deduction of more reliable
#' results from repeated calls to \code{\link{evaluateCycle}}.
#'
#' @param pred,resp \code{Raster*} objects used as predictor and response 
#' domains during EOT analysis.
#' @param n \code{integer}. Number of EOT modes to calculate.
#' @param var \code{numeric}. Minimum amount of variance to be explained. If
#' \code{NULL} (default), all calculated EOT modes are used for
#' \code{\link[remote]{predict}}-ion.
#' @param by \code{integer}. Increment of the sequence passed to
#' \code{\link{seq}}, defaults to \code{24L} for half-monthly input data.
#' @param times \code{integer}. Determines the number of
#' \code{\link{evaluateCycle}} runs, from which average error and regression
#' metrics are subsequently calculated.
#' @param size \code{integer}. Sample size of the training dataset passed to
#' \code{\link{sample}}, defaults to \code{5L} for five years of training.
#' @param cores \code{integer}. Number of cores for parallel processing.
#' @param ... Additional arguments passed to \code{\link{write.csv}}.
#'
#' @return
#' A \code{data.frame}.
#'
#' @seealso 
#' \code{\link{evaluateCycle}}, \code{\link{eot}}.
#' 
#' @author
#' Florian Detsch
#'
#' @export evaluate
#' @name evaluate
evaluate <- function(pred, resp, n = 10L, var = NULL, by = 24L, times = 10L, 
                     size = 5L, cores = 1L, ...) {

  ## evaluate eot-based spatial resampling 'times' times
  lst_scores <- lapply(1:times, function(h) {

    # status message
    cat(ifelse(h == 1, "", "\n"), "Iteration #", h, "...\n", sep = "")

    # indices of training dataset
    indices <- do.call("c", lapply(1:by, function(i) {
      month_id <- seq(i, raster::nlayers(pred), by)

      set.seed(i + h)
      sort(sample(month_id, size))
    }))

    evaluateCycle(pred, resp, training = indices, n, var, cores = cores)
  })

  lst_spatial <- lapply(lst_scores, "[[", 1)
  dat_spatial <- do.call("rbind", lst_spatial)

  ME.se <- plotrix::std.error(dat_spatial$ME, na.rm = TRUE)
  MAE.se <- plotrix::std.error(dat_spatial$MAE, na.rm = TRUE)
  RMSE.se <- plotrix::std.error(dat_spatial$RMSE, na.rm = TRUE)

  dat_spatial <- colMeans(dat_spatial, na.rm = TRUE)
  dat_spatial <- data.frame(ME = dat_spatial[1], ME.se, MAE = dat_spatial[2], MAE.se,
                            RMSE = dat_spatial[3], RMSE.se, Rsq = dat_spatial[5])

  ## if output 'file' is specified, call write.csv()
  dots <- list(...)
  if ("file" %in% names(dots)) {
    dots <- append(list(x = dat_spatial), dots)
    do.call(utils::write.csv, dots)
  }

  return(dat_spatial)
}
