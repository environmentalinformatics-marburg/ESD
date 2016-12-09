#' Visualize Error and Regression Metrics 
#' 
#' @description 
#' \strong{ESD} standard method for visualizing calculated error and regression 
#' metrics.
#' 
#' @param metrics \code{data.frame}. Error and regression metrics as returned by 
#' \code{\link{evaluate}}.
#' @param xlim_err,xlim_rsq \code{numeric}. x-axis range for the lower (errors; 
#' see '...') and upper (\eqn{R^2}) panel.
#' @param left \code{logical}, defaults to \code{TRUE}. Determines position of 
#' the created plot in a multi-panel figure (i.e., if \code{FALSE}, the left 
#' y-axis is removed).
#' @param add \code{logical}, defaults to \code{FALSE}. Determines whether the 
#' created plot is added to an already existing multi-panel figure.
#' @param ... Additional argument passed to \code{\link{round}}. If 'xlim_err' 
#' is missing, the lower axis range is calculated automatically. 
#' 
#' @return 
#' A \code{trellis} object.
#' 
#' @author 
#' Florian Detsch
#' 
#' @examples 
#' \dontrun{
#' ## evaluate performance of EOT-based spatial downscaling
#' metrics <- evaluate(albGIMMS, albMODIS, size = 2)
#' 
#' ## visualize results
#' p <- plotRegressionStats(metrics, xlim_err = c(-.012, 0.052), xlim_rsq = c(0.75, 1.05))
#' p
#' }
#' 
#' @export plotRegressionStats
#' @name plotRegressionStats
plotRegressionStats <- function(metrics, 
                                xlim_err = NULL, xlim_rsq = c(0, 1.05), 
                                left = TRUE, add = FALSE, ...) {

  ## stop if supplied prediction statistics are not named
  nms <- names(metrics)
  if (is.null(nms))
    stop("Please supply named prediction performance statistics as returned by
         Rsenal::regressionStats().")

  # numeric vector to data.frame
  if (is.numeric(metrics)) {
    mat_metrics <- matrix(metrics, 1, byrow = TRUE)
    df_metrics <- data.frame(mat_metrics)
    names(df_metrics) <- names(metrics)
  } else {
    df_metrics <- metrics
  }

  panel.fun <- function(...) {
    lattice::trellis.par.set("clip", list(panel = "off", strip = "off"))
    if (lattice::panel.number() == 1) {
      lmt <- c(floor(xlim_rsq * 10)[1], ceiling(xlim_rsq * 10)[2]) / 10
      lattice::panel.axis("top", at = seq(lmt[1], lmt[2], .1), outside = FALSE,
                          labels = TRUE, half = FALSE, text.cex = .7,
                          tck = .5)
      lattice::panel.axis(side = "left", at = 1, outside = TRUE, tck = .5,
                          labels = ifelse(left, expression("R"["T"]^2), FALSE), text.cex = .7)
      lattice::panel.dotplot(lwd = .5, ...)
      lattice::panel.abline(v = 1, lty = 3, lwd = 1, col = "red")
    }

    lattice::trellis.par.set("clip", list(panel = "off", strip = "off"))
    if (lattice::panel.number() == 2) {
      at <- pretty(xlim_err)
      lattice::panel.axis("bottom", at = at, outside = FALSE, tck = .5,
                          labels = TRUE, half = FALSE, text.cex = .7)
      lattice::panel.axis("left", at = 2:4, outside = TRUE, text.cex = .7, tck = .5,
                          labels = if (left) {
                   c(expression("MAE"["T"]), expression("ME"["T"]),
                     expression("RMSE"["T"]))
                   } else {
                     FALSE
                   }, half = FALSE)
      lattice::panel.abline(v = 0, lty = 3, lwd = 1, col = "red")
      lattice::panel.lines(x = x_se[c(1, 5)], col = "grey60",
                           y = y_se[c(1, 5)], lwd = 4)
      lattice::panel.lines(x = x_se[c(2, 6)], col = "grey60",
                           y = y_se[c(2, 6)], lwd = 4)
      lattice::panel.lines(x = x_se[c(3, 7)], col = "grey60",
                           y = y_se[c(3, 7)], lwd = 4)
      lattice::panel.dotplot(..., lwd = 0.5)
      lattice::panel.dotplot(x = df_plt$fit, y = df_plt$nms,
                             cex = 1, col = "grey20", lwd = 0.5,
                             col.line = "transparent", pch = ifelse(add, 21, 16),
                             fill = ifelse(add, "white", "black"))
    }
  }

  nms <- names(df_metrics)[c(1, 3, 5)]
  nms <- c(nms, "")

  df_rsq <- data.frame(nms = c("Rsq", "", ""), Rsq = c(df_metrics$Rsq, NA, NA))
  df_rsq$nms <- factor(df_rsq$nms, levels = c("Rsq", "", "NULL"))

  rsq_plt <- lattice::dotplot(nms ~ Rsq, data = df_rsq, pch = ifelse(add, 21, 16),
                     xlab = "", ylab = "", col.line = c("grey70", "transparent"),
                     col = "grey20", xlim = xlim_rsq,
                     scales = list(draw = FALSE),
                     cex = 1, as.table = TRUE,
                     par.settings = list(
                       layout.widths = list(left.padding = ifelse(left, 5, 1), right.padding = 0),
                       layout.heights = list(top.padding = 0, bottom.padding = 0)
                     ))

  fit <- c(df_metrics$ME,
           df_metrics$MAE,
           df_metrics$RMSE,
           NA)

  fit_se <- c(df_metrics$ME.se,
              df_metrics$MAE.se,
              df_metrics$RMSE.se,
              NA)

  df_plt <- data.frame(nms = nms,
                       fit = fit,
                       fit_se = fit_se,
                       upr = fit + fit_se,
                       lwr = fit - fit_se)

  x_se <- c(df_plt$lwr, df_plt$upr)
  y_se <- rep(c(2, 1, 3, NA) + 1, 2)

  if (is.null(xlim_err)) {
    xlim_err <- range(x_se, na.rm = TRUE)
    xlim_err <- round(c(xlim_err[1] - 0.2 * xlim_err[1],
                   xlim_err[2] + 0.2 * xlim_err[2]), ...)
  }

  err_lbl <- c("", expression("MAE"["T"]),
               expression("ME"["T"]), expression("RMSE"["T"]))

  err_plt <- lattice::dotplot(nms ~ upr + lwr,
                     data = df_plt,
                     xlab = "", ylab = "",
                     # ylim = c(-1, 4),
                     col = "grey20", col.line = c(rep("grey70", 3), "transparent"),
                     pch = "|", xlim = xlim_err,
                     cex = 1, as.table = TRUE,
                     scales = list(y = list(labels = err_lbl)),
                     par.settings = list(
                       layout.widths = list(left.padding = 4, right.padding = 0),
                       layout.heights = list(top.padding = 0, bottom.padding = 0),
                       axis.components = list(left = list(pad1 = .5),
                                              bottom = list(pad1 = .5),
                                              top = list(pad1 = .5))
                     ))

  lattice::trellis.par.set("clip", list(panel = "off", strip = "off"))
  out_plt <- latticeExtra::resizePanels(Rsenal::latticeCombineGrid(list(rsq_plt, err_plt),
                                             layout = c(1, 2)),
                          h = c(2/6, 4/6))

  out_plt <- update(out_plt, panel = panel.fun)

  return(out_plt)
}
