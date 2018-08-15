# fns_charts.R

# Purpose: For charts of exposure / relativities etc


# Functions in this script:
# fn_rebaseSecondArgToFirstArg
# fn_chartQuotientPctDenom
# fn_chart_meanPredActual
# fn_chart_meanPredActualRel

# devtools::use_package()


#--------------------------------------------------------------------------------
#' Helper function to rebase second arg to first
#' 
#' @param dt_
#' @param firstArg 
#' @param secondArg, 
#' @param ind_rebaseBy (TRUE or FALSE): When or not to rebase (by 'rebaseBy')
#' @param rebaseBy 
#' 
#' @return dataset is changed inplace with _rebased variable added
#' 
fn_rebaseSecondArgToFirstArg <- 
  function(dt_, 
           firstArg, 
           secondArg, 
           ind_rebaseBy,
           rebaseBy){

    colsToSum <- c(firstArg, secondArg)
    summedCols_lbl <- c('firstArg', 'secondArg')
    
    if (ind_rebaseBy) {
      sumsByYear <- dt_[,
                        lapply(.SD, sum),
                        .SDcols = colsToSum,
                        by = rebaseBy]
    } else {
      sumsByYear <- dt_[,
                        lapply(.SD, sum),
                        .SDcols = colsToSum]
    }
    
    for (idx in 1:2){
      setnames(sumsByYear, colsToSum[idx], summedCols_lbl[idx])
    }
    
    sumsByYear[, firstArgOverSecondArg := firstArg / secondArg]
    
    if (ind_rebaseBy){
      #sumsByYear[, uw_year_num := as.numeric(levels(uw_year)[uw_year])]
      #setorder(sumsByYear, uw_year_num)
      sumsByYear <- sumsByYear[, list(rebaseBy, firstArgOverSecondArg)]
      dt_ <- merge(dt_, sumsByYear, by = "rebaseBy", all.x = TRUE)
    } else {
      sumsByYear <- sumsByYear[, list(firstArgOverSecondArg)]
      dt_[, firstArgOverSecondArg := sumsByYear$firstArgOverSecondArg]
    }
    
    setnames(dt_, firstArg, 'firstArg')
    setnames(dt_, secondArg, 'secondArg')
    
    dt_[ , secondArg_rebased := secondArg * firstArgOverSecondArg]
    dt_[ , firstArgOverSecondArg := NULL]
    setnames(dt_, 'firstArg', firstArg)
    setnames(dt_, 'secondArg', secondArg)
    setnames(dt_, 'secondArg_rebased', paste0(secondArg, '_rebased'))
    
    return(dt_)
    
  }


#--------------------------------------------------------------------------------
#' Chart of quotient (eg loss ratio) and %denom (eg % of premium)
#' 
#' Chart of quotient (eg loss ratio) and %denom (eg % of premium)
#' For use prior to any modelling to visualise response by feature.
#' Relevel each year so that if there is a change in mix it does 
#' not mask true pattern.
#' Relevel quotient to a given number (eg loss ratio to client view)
#'
#' @export
#' 
#' @param dt_ a datatable which contains the new and proposed
#' @param x_var The variable for the x-axis
#' @param x_lbl The label for the x-axis 
#' @param x_maxLevels: maximum number of graphed levels for x variable
#' @param denominator_var denominator variable name
#' @param denominator_lbl denominator label
#' @param numerator_var numerator variable name
#' @param numerator_lbl numerator label
#' @param quotient_lbl a label for the resulting quotient
#' @param rebase: Default = TRUE 
#' @param quotient_baseLevel: Second rebase level for the quotient.  
#                             Deault = 1.  If rebase is on, this is always done first
#                             Use can be for client who prefer to view 
#                             an average loss ratio of 75% or whatever, rather 
#                             than 100% where the focus is on relativities only
#' @param rebaseBy: If rebase is one, this option allows user to choose 
#'                          A variable within whose levels the rebase (to 1)
#'
#' @return a ggplot object
#' 
fn_chartQuotientPctDenom <- 
  function(dt_,
           x_var, x_lbl,
           x_maxLevels = NULL,
           denominator_var, denominator_lbl,
           numerator_var, numerator_lbl, 
           quotient_lbl, 
           rebase = TRUE,
           quotient_baseLevel = 1,
           rebaseBy = NULL){
    
    # Rebase within each level of some other variable (eg uw_year) separately?
    ind_rebaseBy <- !is.null(rebaseBy)
    
    #   1. Create a data table with just the columns that we need  
    if (ind_rebaseBy){
      SDcols <- c(rebaseBy, x_var, denominator_var, numerator_var)
      SDcols_lbl <- c('rebaseBy', 'x_var', 'denominator_var', 'numerator_var')
    } else {
      SDcols <- c(x_var, denominator_var, numerator_var)
      SDcols_lbl <- c('x_var', 'denominator_var', 'numerator_var')
    }
    
    # Delete all not needed columns (don't know how to keep wanted ones 'in-place')
    dt_[, setdiff(colnames(dt_), SDcols) := NULL]
    
    for (idx in 1:length(SDcols)){
      setnames(dt_, SDcols[idx], SDcols_lbl[idx])
    }
    
    #   2. Summarise denominator and numerator by x_var (and year)
    if (ind_rebaseBy){
      dt_ <- dt_[,
                      list(denominator_var = sum(denominator_var), 
                           numerator_var = sum(numerator_var)
                      ),
                      by = list(x_var, rebaseBy)]
    } else {
      dt_ <- dt_[,
                      list(denominator_var = sum(denominator_var), 
                           numerator_var = sum(numerator_var)
                      ),
                      by = list(x_var)]
    }
    rm(dt_)
    
    #   3. Rebase denominator_var to numerator_var 
    dt_ <- fn_rebaseSecondArgToFirstArg(
      dt_ = dt_, 
      firstArg = 'numerator_var', 
      secondArg = 'denominator_var',
      ind_rebaseBy = ind_rebaseBy)
    setorder(dt_, x_var)
    
    # Note: For some reason dt_ can lose it's auto-print property and need
    #       to be printed to be seen rather than just typing dt_
    # print(dt_)
    
    #   3. Sum across all rebaseBy (if we are using it) and add to by-year data
    if (ind_rebaseBy){
      dt_allRebaseBy <- dt_[,
                            list(numerator_var = sum(numerator_var), 
                                 denominator_var_rebased = sum(denominator_var_rebased)
                            ),
                            by = x_var]
      dt_allRebaseBy[['rebaseBy']] = 'All'
      dt_ <- rbind(dt_, dt_allRebaseBy)
      setorder(dt_, rebaseBy, x_var)
    }
    
    #   4. Create quotient and rebased quotient
    
    # If there is no rebasing just use the simple quotient 
    if (rebase){
      dt_[, quotient := round(100 * quotient_baseLevel * numerator_var / denominator_var_rebased,0)]
    } else {
      dt_[, quotient := numerator_var / denominator_var]
    }
    
    if (ind_rebaseBy){
      dt_quotient <- as.data.table(cast(dt_, x_var  ~ rebaseBy, value = 'quotient'))
    } else {
      dt_quotient <- dt_[ , list(x_var, All = quotient)]
    }
    
    #   5. Create % of denom (e.g. % of exposure) for each value of x_var
    if (ind_rebaseBy){
      dt_denom <- as.data.table(cast(dt_[uw_year == 'All', ], 
                                     x_var  ~ uw_year, 
                                     value = 'denominator_var_rebased'))
      tot_denom <- sum(dt_denom$All)
      dt_denom[, pctOfDenom := round(100 * All / tot_denom, 0.5)]
      dt_denom[, All := NULL]
    } else {
      tot_denom <- sum(dt_$denominator_var_rebased)
      dt_denom <- dt_[, list(pctOfDenom = round(100 * denominator_var_rebased / tot_denom, 0.5) )]
    }
    
    dt_ <- cbind(dt_quotient, dt_denom)
    rm(dt_quotient, dt_denom)
    
    
    #   6. Limit xvar to x_maxLevels (if x_maxLevels is not null)
    #      Use the levels with most exposure
    #      Only apply maxLevels for character or factors
    #      So, if neither, set x_maxLevels to NULL
    if (!(is.factor(dt_$x_var) | is.factor(is.character))){
      x_maxLevels <- NULL
    }
    
    if (!is.null(x_maxLevels)){
      # If dt_ has less than x_maxLevels then set x_maxLevels to this amount
      # could have done this in the if statement above, but prefer not to 
      # rely on is.null being evaluated first
      x_maxLevels <- min(x_maxLevels, dim(dt_)[1])
      idx_maxLevels <- order(dt_$pctOfDenom, decreasing = TRUE)[1:x_maxLevels]
      
      # Return dt_ in same order as originally; so sort the index
      expTot <- sum(dt_$pctOfDenom)
      dt_ <- dt_[sort(idx_maxLevels), ]
      expUsed <- sum(dt_$pctOfDenom) / expTot
      rm(idx_maxLevels, expTot)
    } else {
      expUsed <- 1
    }
    
    #   7. Choose scale for yaxis based on value of quotient and then
    #      rebase pctOfDenom so that 100% would be top of the yaxis scale 
    #      y axis always starts from zero
    #      Reduce this to visually be lower if some exposure was left out
    #      when limiting to x_maxLevels
    
    # for now just fix ymax to whatever it needs to be to print everything
    ymax <- 1.05 * max(dt_$All[is.finite(dt_$All)])
    
    # max denom should be half way up yaxis; reduced if not all exposure is shown
    rebase_pctDenom <- expUsed * (ymax / 2) / max(dt_$pctOfDenom)
    dt_[, pctOfDenom := pctOfDenom * rebase_pctDenom]
    
    # deal with NA in xvar
    dt_[is.na(x_var), x_var := 'NA']
    dt_[!is.finite(x_var), x_var := 'NA']
    
    
    #   8. Choose angle and for text on x-axis
    angle = 0; hjust = 0.5;
    maxLen <- 1 # Default for numerics
    if (is.factor(dt_$x_var)) {maxLen <-  max(nchar(levels(dt_$x_var)))}
    if (is.character(dt_$x_var)) {maxLen <-  max(nchar(dt_$x_var))}
    if (maxLen > 1){
      angle = 5; hjust = 1;
    }
    if (maxLen > 4){
      angle = 30; hjust = 1;
    }
    
    # Note: bars are written first so that they don't cover the points
    gg_1 <- 
      ggplot(dt_, aes(x = x_var)) + 
      geom_bar(aes(y = pctOfDenom), stat="identity", fill = 'yellow') +
      geom_point(aes(y = All), colour = "black") + 
      xlab(x_lbl) +
      ylab(quotient_lbl) + ylim(c(0, ymax)) + 
      theme_set(theme_gray(base_size = 20)) +
      theme(legend.title = element_blank(), legend.position = "top",
            axis.text.x = element_text(angle = angle, hjust = hjust)) 
    
    if (grepl("decile",x_var)) {
      gg_1 <- gg_1 + scale_x_continuous(breaks = 1:10)}
    
    rm(dt_, x_lbl)
    return(gg_1)
  }


#--------------------------------------------------------------------------------
#' Chart of mean actual and preditions
#'
#' @export
#' 
#' @param fname_data the filename for the data file
#' @param fname_vars the file name for variable information
#' @param model_ref the reference name for the model
#' @param x_var the variable for the x-axis
#' @param x_lbl the label for the x-axis 
#' @param x_maxLevels maximum number of graphed levels for x variable
#' @param denominator_var denominator variable name
#' @param denominator_lbl denominator label
#' @param numerator_var numerator variable name
#' @param numerator_lbl numerator label
#' @param quotient_lbl a label for the resulting quotient
#' @param rebase: Default = TRUE     
#' @param quotient_baseLevel: Second rebase level for the quotient.  
#'                             Deault = 1.  If rebase is on, this is always done first
#'                             Use can be for client who prefer to view 
#'                             an average loss ratio of 75% or whatever, rather 
#'                             than 100% where the focus is on relativities only
#' @param missValue
#' @param print
#' @param save
#' @param verbose
#'
#' @return a ggplot object
#'
fn_chart_meanPredActual <- 
  function(fname_data, 
           fname_vars, 
           model_ref,
           x_var, x_lbl,
           x_maxLevels = NULL,
           denominator_var, denominator_lbl,
           numerator_var, numerator_lbl, 
           quotient_lbl, 
           rebase = TRUE, quotient_baseLevel = 1,
           missValue = NULL,
           print = TRUE,
           save = TRUE,
           verbose = FALSE){
    
    if(verbose){print('fn_chart_meanPredActual: start')}
    
    if(verbose){print('fn_chart_meanPredActual: loading data...')}
    # load data and variables
    load(file = file.path(dirRData, paste0(fname_dt_all, '.RData')))
    
    # dt_preds, idx_allpreds
    load(file = file.path(dirRData, 
                          paste0('04a_dt_preds_', model_prefix, '.RData'))
    )
    load(file = file.path(dirRData, paste0(fname_vars, '.RData')))
    load(file = file.path(dirRData, paste0(model_ref, '_', 'run_params.RData')))

    # define dt_
    idx_ <- intersect(idx_allpreds, which(dt_all$fold %in% folds_test))
    dt_ <- copy(dt_all[idx_])
    rm(idx_)
    
    modelData <- dt_preds[fold %in% folds_test, model_ref, with = FALSE]
    
    #   1. Create a data table with just the columns that we need  
    if(verbose){print('fn_chart_meanPredActual: extract columns...')}
    SDcols     <- c(x_var,   denominator_var,   numerator_var)
    SDcols_lbl <- c('x_var', 'denominator_var', 'numerator_var')
    
    # Delete all not needed columns (don't know how to keep wanted ones 'in-place')
    dt_[, setdiff(colnames(dt_), SDcols) := NULL]
    
    for (idx in 1:length(SDcols)){
      setnames(dt_, SDcols[idx], SDcols_lbl[idx])
    }
    dt_[, var_target := modelData]
    if(!is.null(missValue)){
      dt_ <- dt_[x_var != -999]
    }
    
    #   2. Summarise denominator and numerator by x_var (and year)
    if(verbose){print('fn_chart_meanPredActual: summarise by x_var...')}
    dt_ <- dt_[,
                    list(denominator_var = sum(denominator_var), 
                         numerator_var = sum(numerator_var),
                         var_target = sum(var_target)
                    ),
                    by = list(x_var)]
    
    #   3. Rebase denominator_var to numerator_var 
    if(verbose){print('fn_chart_meanPredActual: rebase...')}
    dt_ <- fn_rebaseSecondArgToFirstArg(
      dt_ = dt_, 
      firstArg = 'numerator_var', 
      secondArg = 'denominator_var',
      ind_rebaseBy = FALSE)
    dt_ <- fn_rebaseSecondArgToFirstArg(
      dt_ = dt_, 
      firstArg = 'numerator_var', 
      secondArg = 'var_target',
      ind_rebaseBy = FALSE)
    
    setorder(dt_, x_var)
    
    # Note: For some reason dt_ can lose it's auto-print property and need
    #       to be printed to be seen rather than just typing dt_
    # print(dt_)
    
    
    #   4. Create quotient and rebased quotient
    #      rebase to a specific number if required, otherwise
    #      leave it rebased at 1 
    if (rebase){
      dt_[, response_pct := round(100 * quotient_baseLevel * numerator_var / denominator_var_rebased,0)]
      dt_[, model_pct := round(100 * quotient_baseLevel * var_target / denominator_var_rebased,0)]
    } else {
      dt_[, response_pct := numerator_var / denominator_var]
      dt_[, model_pct := var_target / denominator_var]
    }
    
    dt_quotient <- dt_[ , list(x_var, response_pct, model_pct)]
    
    #   5. Create % of denom (e.g. % of exposure) for each value of x_var
    tot_denom <- sum(dt_$denominator_var_rebased)
    dt_denom <- dt_[, list(pctOfDenom = round(100 * denominator_var_rebased / tot_denom, 0.5) )]
    
    dt_ <- cbind(dt_quotient, dt_denom)
    rm(dt_quotient, dt_denom)
    
    #   6. Limit xvar to x_maxLevels (if x_maxLevels is not null)
    #      Use the levels with most exposure
    #      Only apply maxLevels for character or factors
    #      So, if neither, set x_maxLevels to NULL
    if (!(is.factor(dt_$x_var) | is.factor(is.character))){
      x_maxLevels <- NULL
    }
    
    if (!is.null(x_maxLevels)){
      # If dt_ has less than x_maxLevels then set x_maxLevels to this amount
      # could have done this in the if statement above, but prefer not to 
      # rely on is.null being evaluated first
      x_maxLevels <- min(x_maxLevels, dim(dt_)[1])
      idx_maxLevels <- order(dt_$pctOfDenom, decreasing = TRUE)[1:x_maxLevels]
      
      # Return dt_ in same order as originally; so sort the index
      expTot <- sum(dt_$pctOfDenom)
      dt_ <- dt_[sort(idx_maxLevels), ]
      expUsed <- sum(dt_$pctOfDenom) / expTot
      rm(idx_maxLevels, expTot)
    } else {
      expUsed <- 1
    }
    
    #   7. Choose scale for yaxis based on value of quotient and then
    #      rebase pctOfDenom so that 100% would be top of the yaxis scale 
    #      y axis always starts from zero
    #      Reduce this to visually be lower if some exposure was left out
    #      when limiting to x_maxLevels
    
    # for now just fix ymax to whatever it needs to be
    # to print all response
    # probably need to make this bit better
    ymax <- 1.05 * max(dt_$response_pct[is.finite(dt_$response_pct)])
    
    # max denom should be half way up yaxis; reduced if not all exposure is shown
    rebase_pctDenom <- expUsed * (ymax / 2) / max(dt_$pctOfDenom)
    dt_[, pctOfDenom := pctOfDenom * rebase_pctDenom]
    
    # deal with NA in xvar
    dt_[is.na(x_var), x_var := 'NA']
    dt_[!is.finite(x_var), x_var := 'NA']
    
    #   8. Choose angle and for text on x-axis
    angle = 0; hjust = 0.5;
    maxLen <- 1 # Default for numerics
    if (is.factor(dt_$x_var)) {maxLen <-  max(nchar(levels(dt_$x_var)))}
    if (is.character(dt_$x_var)) {maxLen <-  max(nchar(dt_$x_var))}
    if (maxLen > 1){
      angle = 5; hjust = 1;
    }
    if (maxLen > 4){
      angle = 30; hjust = 1;
    }
    
    # Note: bars are written first so that they don't cover the points
    if(verbose){print('fn_chart_meanPredActual: create graph...')}
    gg_1 <- 
      ggplot(dt_, aes(x = x_var)) + 
      geom_bar(aes(y = pctOfDenom), stat="identity", fill = 'yellow') +
      geom_point(aes(y = response_pct, colour = "Actual reponse"), size = 1.6) + 
      xlab(x_lbl) +
      ylab(quotient_lbl) + ylim(c(0, ymax)) + 
      theme_set(theme_gray(base_size = 20)) +
      theme(legend.title = element_blank(), legend.position = "top",
            axis.text.x = element_text(angle = angle, hjust = hjust)) 
    
    if (grepl("decile",x_var)) {
      gg_1 <- gg_1 + scale_x_continuous(breaks = 1:10)}
    
    if (is.factor(dt_$x_var)){
      gg_1 <- gg_1 + 
        geom_point(aes(y = model_pct, colour = "Fitted model"), size = 0.8) 
    } else {
      gg_1 <- gg_1 + 
        geom_line(aes(y = model_pct, colour = "Fitted model"))
    }
    
    rm(dt_, x_lbl)
    
    # to print
    if(print) {
      if(verbose){print('fn_chart_meanPredActual: printing graph...')}
      print(gg_1)}
    
    # to save results
    if(save){
      if(verbose){print('fn_chart_meanPredActual: saving graph...')}
      pdf(file.path(dirROutput, 
                    paste0('04r_meanPreds_', model_prefix, '_', x_var, '.pdf')), 
          width=6, height=5)
      print(gg_1)
      dev.off()
      }
    
    #return(gg_1)
  }


#--------------------------------------------------------------------------------
#' Chart of actual and fitted relativities
#' 
#' Chart of actual and fitted relativities
#' Relevel each year so that if there is a change in mix it does 
#' not mask true pattern.
#' Relevel quotient to a given number (eg loss ratio to client view)
#'
#' @export
#' 
#' @param fname_data the filename for the data file
#' @param fname_vars the file name for variable information
#' @param model_ref the reference name for the model
#' @param x_var the variable for the x-axis 
#' @param x_lbl the variable and label for the x-axis 
#' @param var_target the variable name for the model predictions
#' @param modelData 
#' @param x_maxLevels maximum number of graphed levels for x variable
#' @param denominator_var denominator variable name
#' @param denominator_lbl denominator label
#' @param numerator_var numerator variable name
#' @param numerator_lbl numerator label
#' @param quotient_lbl a label for the resulting quotient
#' @param rebase (default: TRUE)     
#' @param quotient_baseLevel: Second rebase level for the quotient.  
#                             Deault = 1.  If rebase is on, this is always done first
#                             Use can be for client who prefer to view 
#                             an average loss ratio of 75% or whatever, rather 
#                             than 100% where the focus is on relativities only
#' @param dt_rel_num a data table with the relativities for numerics
#' @param dt_rel_cat a data table with the relativities for factors
#' @param linkfn
#' @param ymaxlower
#' @param ymaxupper
#' @param print
#' @param save
#' @param verbose
#'
#' @return a ggplot object
#' 
fn_chart_meanPredActualRel <- 
  function(fname_data, 
           fname_vars, 
           model_ref,
           x_var, x_lbl,
           var_target, modelData,
           x_maxLevels = NULL,
           denominator_var, denominator_lbl,
           numerator_var, numerator_lbl, 
           quotient_lbl, 
           rebase = TRUE, quotient_baseLevel = 1,
           missValue = NULL,
           dt_rel_num = NULL,
           dt_rel_cat = NULL,
           linkfn = NULL,
           ymaxlower = NULL,
           ymaxupper = NULL,
           print = TRUE,
           save = TRUE,
           verbose = FALSE
  ){
    
    if(verbose){
      print('fn_chart_meanPredActualRel: start')
      }
    
    if(verbose){
      print('fn_chart_meanPredActualRel: loading data...')
      }
    # load data and variables
    load(file = file.path(dirRData, paste0(fname_dt_all, '.RData')))
    
    # dt_preds, idx_allpreds
    load(file = file.path(dirRData, 
                          paste0('04a_dt_preds_', model_prefix, '.RData'))
    )
    load(file = file.path(dirRData, paste0(fname_vars, '.RData')))
    load(file = file.path(dirRData, paste0(model_ref, '_', 'run_params.RData')))
    
    # define dt_
    idx_ <- intersect(idx_allpreds, which(dt_all$fold %in% folds_test))
    dt_ <- copy(dt_all[idx_])
    rm(idx_)
    
    modelData <- dt_preds[fold %in% folds_test, model_ref, with = FALSE]
    
    #   1. Create a data table with just the columns that we need  
    SDcols <- c(x_var, denominator_var, numerator_var)
    SDcols_lbl <- c('x_var', 'denominator_var', 'numerator_var')
    
    # Delete all not needed columns (don't know how to keep wanted ones 'in-place')
    dt_[, setdiff(colnames(dt_), SDcols) := NULL]
    
    for (idx_col in 1:length(SDcols)){
      setnames(dt_, SDcols[idx_col], SDcols_lbl[idx_col])
    }
    rm(idx_col)
    dt_[, var_target := modelData]
    if(!is.null(missValue)){
      dt_ <- dt_[x_var != -999]
    }
    
    #   2. Summarise denominator and numerator by x_var (and year)
    # In original code where x_var is continuous this just created
    # a data table for each point - if there are lots of points this
    # takes a lot of memory to store an print.  Also if binom - just get
    # loads of zeros (if unbalanced classes)
    # So we will now add - that for numeric classes, if there are more 
    # than 50? unique classes, replace x_var with mean of quantile
    # relativities
    if (is.numeric(dt_$x_var)){
      if (length(unique(dt_$x_var)) > 50){
        brks <- unique(as.vector(quantile(dt_$x_var, probs = (0:49)/49)))
        ints <- findInterval(dt_$x_var, vec = brks, all.inside = TRUE)
        dt_[, x_var := (brks[ints] + brks[ints + 1]) / 2]
        rm(brks, ints)
      }
    }
    dt_ <- dt_[,
                    list(denominator_var = sum(denominator_var), 
                         numerator_var = sum(numerator_var),
                         var_target = sum(var_target)
                    ),
                    by = list(x_var)]
    
    #rm(dt_)
    
    #   3. Rebase denominator_var to numerator_var 
    dt_ <- fn_rebaseSecondArgToFirstArg(
      dt_ = dt_, 
      firstArg = 'numerator_var', 
      secondArg = 'denominator_var',
      ind_rebaseBy = FALSE)
    dt_ <- fn_rebaseSecondArgToFirstArg(
      dt_ = dt_, 
      firstArg = 'numerator_var', 
      secondArg = 'var_target',
      ind_rebaseBy = FALSE)
    
    setorder(dt_, x_var)
    
    # Note: For some reason dt_ can lose it's auto-print property and need
    #       to be printed to be seen rather than just typing dt_
    # print(dt_)
    
    
    #   4. Create quotient and rebased quotient
    
    # rebase to a specific number if required, otherwise
    # leave it rebased at 1 
    if (rebase){
      dt_[, response_pct := round(100 * quotient_baseLevel * numerator_var / denominator_var_rebased,0)]
      dt_[, model_pct := round(100 * quotient_baseLevel * var_target / denominator_var_rebased,0)]
    } else {
      dt_[, response_pct := numerator_var / denominator_var]
      dt_[, model_pct := var_target / denominator_var]
    }
    
    dt_quotient <- dt_[ , list(x_var, response_pct, model_pct)]
    
    #   5. Create % of denom (e.g. % of exposure) for each value of x_var
    tot_denom <- sum(dt_$denominator_var_rebased)
    dt_denom <- dt_[, list(pctOfDenom = round(100 * denominator_var_rebased / tot_denom, 0.5) )]
    
    dt_ <- cbind(dt_quotient, dt_denom)
    rm(dt_quotient, dt_denom)
    
    
    #   6. Limit xvar to x_maxLevels (if x_maxLevels is not null)
    #      Use the levels with most exposure
    #      Only apply maxLevels for character or factors
    #      So, if neither, set x_maxLevels to NULL
    if (!(is.factor(dt_$x_var) | is.factor(is.character))){
      x_maxLevels <- NULL
    }
    
    if (!is.null(x_maxLevels)){
      # If dt_ has less than x_maxLevels then set x_maxLevels to this amount
      # could have done this in the if statement above, but prefer not to 
      # rely on is.null being evaluated first
      x_maxLevels <- min(x_maxLevels, dim(dt_)[1])
      idx_maxLevels <- order(dt_$pctOfDenom, decreasing = TRUE)[1:x_maxLevels]
      
      # Return dt_ in same order as originally; so sort the index
      expTot <- sum(dt_$pctOfDenom)
      dt_ <- dt_[sort(idx_maxLevels), ]
      expUsed <- sum(dt_$pctOfDenom) / expTot
      rm(idx_maxLevels, expTot)
    } else {
      expUsed <- 1
    }
    
    #   7. Choose scale for yaxis based on value of quotient and then
    #      rebase pctOfDenom so that 100% would be top of the yaxis scale 
    #      y axis always starts from zero
    #      Reduce this to visually be lower if some exposure was left out
    #      when limiting to x_maxLevels
    
    # for now just fix ymax to whatever it needs to be
    # to print all model + 20%
    # probably need to make this bit better
    ymax <- 1.2 * max(dt_$model_pct[is.finite(dt_$model_pct)])
    if(!is.null(ymaxlower)) ymax <- max(ymaxlower, ymax) 
    if(!is.null(ymaxupper)) ymax <- min(ymaxupper, ymax) 
    
    # max denom should be half way up yaxis; reduced if not all exposure is shown
    rebase_pctDenom <- expUsed * (ymax / 2) / max(dt_$pctOfDenom)
    dt_[, pctOfDenom := pctOfDenom * rebase_pctDenom]
    
    # rebase relativity if we have it so it shows on the graph
    # this is a bit complicated.  we need to find the mean level
    # of the graph find the linear pred that fits for that and
    # then mean sure then mean lin_pred is at that level
    # and then turn the lin pred into a probability
    if (x_var %in% unique(dt_rel_num$varName)){
      mean_pct <- sum(dt_$model_pct * dt_$pctOfDenom) / sum(dt_$pctOfDenom)
      if (linkfn == 'logit'){
        mean_linearPred <- log(mean_pct / (1 - mean_pct))
      } else if (linkfn == 'log'){
        mean_linearPred <- log(mean_pct)
      }
      dt_rel_ <- dt_rel_num[varName == x_var]
      mean_rel <- mean(dt_rel_$linear_pred)
      dt_rel_[, linear_pred := linear_pred - mean_rel + mean_linearPred]
      if (linkfn == 'logit'){
        dt_rel_[, model_prob := 1 / (1 + exp(-linear_pred))]
      } else if (linkfn == 'log'){
        dt_rel_[, model_prob := exp(linear_pred)]
      }
    }
    if (x_var %in% unique(dt_rel_cat$varName)){
      mean_pct <- sum(dt_$model_pct * dt_$pctOfDenom) / sum(dt_$pctOfDenom)
      if (linkfn == 'logit'){
        mean_linearPred <- log(mean_pct / (1 - mean_pct))
      } else if (linkfn == 'log'){
        mean_linearPred <- log(mean_pct)
      }
      dt_rel_ <- dt_rel_cat[varName == x_var]
      #dt_rel_[linear_pred > 0, ]
      dt_rel_ <- dt_rel_[cat %in% levels(dt_$x_var)[dt_$x_var]]
      mean_rel <- mean(dt_rel_$linear_pred)
      dt_rel_[, linear_pred := linear_pred - mean_rel + mean_linearPred]
      if (linkfn == 'logit'){
        dt_rel_[, model_prob := 1 / (1 + exp(-linear_pred))]
      } else if (linkfn == 'log'){
        dt_rel_[, model_prob := exp(linear_pred)]
      }
    }
    # deal with NA in xvar
    dt_[is.na(x_var), x_var := 'NA']
    dt_[!is.finite(x_var), x_var := 'NA']
    
    #   8. Choose angle and for text on x-axis
    angle = 0; hjust = 0.5;
    maxLen <- 1 # Default for numerics
    if (is.factor(dt_$x_var)) {maxLen <-  max(nchar(levels(dt_$x_var)))}
    if (is.character(dt_$x_var)) {maxLen <-  max(nchar(dt_$x_var))}
    if (maxLen > 1){
      angle = 5; hjust = 1;
    }
    if (maxLen > 3){
      angle = 30; hjust = 1;
    }
    
    # Note: bars are written first so that they don't cover the points
    gg_1 <- 
      ggplot(dt_, aes(x = x_var)) + 
      theme_set(theme_gray(base_size = 12)) +
      geom_bar(aes(y = pctOfDenom), stat="identity", fill = 'yellow') +
      geom_point(aes(y = response_pct, colour = "Actual"), size = 1.6) + 
      xlab(x_lbl) +
      ylab(quotient_lbl) + ylim(c(0, ymax)) + 
      theme(legend.title = element_blank(), legend.position = "top",
            axis.text.x = element_text(angle = angle, hjust = hjust)) 
    
    if (grepl("decile",x_var)) {
      gg_1 <- gg_1 + scale_x_continuous(breaks = 1:10)}
    
    if (is.factor(dt_$x_var)){
      gg_1 <- gg_1 + 
        geom_point(aes(y = model_pct, colour = "Fitted"), size = 0.8) 
    } else {
      gg_1 <- gg_1 + 
        geom_line(aes(y = model_pct, colour = "Fitted"))
    }
    
    if (x_var %in% unique(dt_rel_num$varName)){
      gg_1 <- gg_1 + 
        geom_line(data = dt_rel_, 
                  aes(x = value,
                      y = model_prob, colour = "Relativities"))
      
    }
    if (x_var %in% unique(dt_rel_cat$varName)){
      gg_1 <- gg_1 + 
        geom_point(data = dt_rel_, 
                   aes(x = cat,
                       y = model_prob, colour = "Relativities"))
      
    }
    
    rm(dt_, x_lbl)
    return(gg_1)
  }


