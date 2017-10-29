# fns_performanceCharts.R


#-----------------------------------------------------------------------

#' Actual versus expected by quantile of prediction
#'
#' Actual versus expected by quantile of prediction
#'
#' @export
#'
#' @param dt_toUse A data table containing the variables below and "fold"
#' @param train_folds vector of the training folds
#' @param val_folds vector of the validation folds
#' @param actual The variable name for Actual
#' @param predicted The variable name for the prediction or score
#' @param ex Exposure measure to divide the sum of predictions at each qtile.  If NULL
#'         this is assumed to be 1 for each example
#' @param qtiles If not NULL, these are used for the breaks
#' @param bln_abline If TRUE, a line x = y will be added
#' @param dirROutput the output directory
#'
#' @return Nothing returned to R, but graph output to dirROutput
#'
fn_qtileAvE <- function(dt_toUse,
                        train_folds, val_folds,
                        actual, predicted,
                        ex = NULL,
                        qtiles = NULL,
                        bln_abline = FALSE,
                        dirROutput){

  setnames(dt_toUse, actual, "actual")
  setnames(dt_toUse, predicted, "predicted")

  dt_toUse[fold %in% train_folds, trainVal := "train"]
  dt_toUse[fold %in% val_folds,   trainVal := "val"]

  if (is.null(ex)) {
    dt_toUse[, ('ex') := 1]}

  # add qtiles to dt_results
  if (is.null(qtiles)){
    qtiles <- quantile(dt_toUse[fold %in% 1:10, predicted],
                       probs = (0:10)/10,
                       na.rm = TRUE)
  }

  qtileLabels <- cut(dt_toUse$predicted,
                     breaks = qtiles,
                     labels = FALSE,
                     include.lowest = TRUE)

  dt_toUse[, predQtile := qtileLabels]

  dt_AvE <- dt_toUse[, list(n_examples = .N,
                            actual = sum(actual),
                            predicted = sum(predicted),
                            ex = sum(ex)),
                     by = c("trainVal", "predQtile")]

  dt_AvE_val <- dt_AvE[trainVal == "val"]
  dt_AvE_val$n_pct <- dt_AvE_val$n_examples / sum(dt_AvE_val$n_examples)
  dt_AvE_val$n_pct <- dt_AvE_val$n_pct * 0.75 / max(dt_AvE_val$n_pct)
  dt_AvE[, meanActual := actual / ex]
  dt_AvE[, meanPredicted := predicted / ex]

  setorder(dt_AvE, trainVal, predQtile)

  dt_Ave_gg <- melt(dt_AvE,
                    id.vars = c("predQtile", 'trainVal'),
                    measure.vars = c('meanActual', 'meanPredicted'))

  dt_Ave_gg <- dcast(dt_Ave_gg,
                     predQtile + trainVal ~ variable)

  gg_1 <- ggplot(dt_Ave_gg, aes(x = predQtile, y = meanActual)) +
    geom_line(aes(col = trainVal), lwd = 1.5) +
    geom_col(data = dt_AvE_val, aes(x = predQtile, y = n_pct)) +
    xlab('Predicted Percentile') +
    ylab('Mean actual') +
    theme_set(theme_gray(base_size = 16)) +
    theme(legend.title = element_blank(), legend.position = "top")

  if(bln_abline){
    gg_1 <- gg_1 + geom_abline(intercept = 0, slope = 1)
  }

  pdf(file.path(dirROutput, '10b_gg_AvEqtiles_v2.pdf'))
  print(gg_1)
  dev.off()

}


