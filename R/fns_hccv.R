#fns_hccv.R

# Purpose: Various functions for high cardinality categorical variables


# Functions in this script:

# fn_hc

# add to namespace..
# devtools::use_package("")


#--------------------------------------------------------------------------------
#' Logit function
#' 
#' Logit function to create offset when the mean prediction from a model is known
#' 
#' @export
#'
#' @param x The mean prediction for which the offset (using the logit) is required 
#' 
fn_logit <- function(x){
  probsToUse <- copy(x)
  probsToUse[probsToUse == 0] <- exp(-10)
  probsToUse[probsToUse == 1] <- 1 - exp(-10)
  log(probsToUse / (1 - probsToUse ))
}

#--------------------------------------------------------------------------------
#' Works on one hccv to return a data table of scores
#' 
#' Works on one hccv to return a data table of scores
#' 
#' @export
#'
#' @param dt_all the name of the dataset
#' @param highCardVar a vector of the hccv's
#' @param targetVar the target variable
#' @param exposureVar the exposure variable
#' @param foldVar the fold variable
#' @param foldsWithTarget folds which have the target
#' 
#' @return
#'
fn_hc <- function(dt_all, highCardVar,
                  targetVar, exposureVar, foldVar,
                  foldsWithTarget){
  # dt_hc: from dt_all with the hccv on
  # dt_hc_fullList: unique list of all hccvs
  # dt_results_folds: to store results for each fold
  # dt_results_allData: to store average results across all folds
   
  # dt_hc_exfold: The hcs data but excluding current fold
  # dt_hc_fold  : The hcs data for the current fold only
  
  # dt_hc_exfold_summary: Detail summarised to hc level for the glm
  
  foldsAll <- sort(unique(dt_all[[foldVar]]))
  
  dt_hc <- dt_all[, c('fold', targetVar, exposureVar, highCardVar),
                  with = FALSE]
  
  setnames(dt_hc, targetVar, 'response')
  setnames(dt_hc, exposureVar, 'ex')
  
  # initialise offset. If we have non hccvs to use, create offset using glm 
  dt_hc[, predResponse_mean := sum(response) / sum(ex)]
  
  dt_hc_fullList <- unique(dt_hc[, highCardVar, with = FALSE])
  setorder(dt_hc_fullList) # with no args, all columns are ordered
  
  n_unique <- nrow(dt_hc_fullList)
  dt_hcAndfold_fullList <- dt_hc_fullList[rep(seq_len(n_unique), length(foldsAll))]
  dt_hcAndfold_fullList[, fold := rep(1:(length(foldsAll)), each = n_unique)]
  

  # 2. Set up results tables
  dt_results_folds <- copy(dt_hcAndfold_fullList)
  dt_results_folds[, score := NA_real_]
  
  dt_results_allData <- copy(dt_hc_fullList)
  dt_results_allData[, score := NA_real_]
  
  #  3. Main analysis loop
  library(doParallel)
  cl <- parallel::makeCluster(10)
  doParallel::registerDoParallel(cl)
  
  for (idx_fold in foldsWithTarget){
    # idx_fold = 1
    print(idx_fold)
    
    dt_hc_exfold <- dt_hc[fold != idx_fold]
    dt_hc_fold <- dt_hc[fold == idx_fold]
    
    # Summarise data by the high cat variable (and fold) for glm
    dt_hc_exfold_summary <- dt_hc_exfold[, 
                                         list(ex = sum(ex),
                                              response = sum(response),
                                              predResponse_mean = sum(ex * predResponse_mean) 
                                              / sum(ex)
                                         ),
                                         by = c(highCardVar, "fold")]
    
    setorderv(dt_hc_exfold_summary, highCardVar)
    
    dt_hc_exfold_summary[, proportion_1 :=  pmin(1, response  / ex)]
    dt_hc_exfold_summary[, proportion_0 :=  1 - proportion_1]
    dt_hc_exfold_summary[, n_examples :=  round(ex, 0)]
    
    dt_hc_exfold_summary[, offset := fn_logit(predResponse_mean)]
    
    fmla_1 <- 
      as.formula(paste0("response ~ ", highCardVar))
    
    modMat_summary <- MatrixModels::model.Matrix(fmla_1,
                                                 data = dt_hc_exfold_summary,
                                                 sparse = TRUE,
                                                 drop.unused.levels = FALSE)[,-1]
    
    y_mat <- as.matrix(dt_hc_exfold_summary[, list(proportion_0, proportion_1)])
    
    set.seed(2017)
    time_glmnet <-
      system.time(
        glmnet_ <- 
          glmnet::cv.glmnet(x = modMat_summary,
                            y = y_mat,
                            family = 'binomial',
                            weights = dt_hc_exfold_summary$n_examples,
                            offset = dt_hc_exfold_summary$offset,
                            standardize = TRUE,
                            parallel = TRUE,
                            alpha = 0.5,
                            nfold = 10
          )
      )
    print(time_glmnet)
    
    
    # Score on detail training data
    modMat_detail <- 
      MatrixModels::model.Matrix(fmla_1,
                                 data = dt_hc_exfold,
                                 sparse = TRUE,
                                 drop.unused.levels = FALSE)[,-1]
    
    
    predResponse_ <- predict(object = glmnet_,
                             newx = modMat_detail,
                             offset = fn_logit(dt_hc_exfold$predResponse_mean),
                             type = "response",
                             s = "lambda.1se") %>% 
      as.numeric()
    
    dt_hc_exfold[, predResponse_mean := predResponse_]
    rm(predResponse_)
    
    dt_scores_fold <- dt_hc_exfold[, list(score = first(predResponse_mean)),
                                   by = highCardVar]
    setorder(dt_scores_fold)
    
    
    dt_results_thisfold <- dt_results_folds[fold == idx_fold]
    dt_results_otherfold <- dt_results_folds[fold != idx_fold]
    
    dt_results_thisfold[, score := NULL]
    dt_results_thisfold <- 
      merge(dt_results_thisfold,
            dt_scores_fold,
            by = highCardVar)
    
    dt_results_folds <- rbind(dt_results_thisfold, dt_results_otherfold)
    rm(dt_results_thisfold, dt_results_otherfold)
    
    setorderv(dt_results_folds, c(highCardVar, "fold"))
  }
  
  stopCluster(cl)
  registerDoSEQ()
  
  # rebase average predictions in each fold to 0 so that the relative score for a fold
  # is not simply higher than that in another due randomness in the intercept 
  dt_temp <- dt_results_folds[,list(foldMeanScore = mean(score, na.rm = TRUE)),
                              by = fold]
  dt_results_folds <- merge(dt_results_folds,
                            dt_temp,
                            by = "fold",
                            all.x = TRUE,
                            sort = FALSE)
  dt_results_folds[fold %in% foldsWithTarget, score := score - foldMeanScore]
  dt_results_folds[(fold %in% foldsWithTarget) & is.na(score), score := 0]
  
  dt_results_folds[, foldMeanScore := NULL]
  rm(dt_temp)
  
  # I prefer to take the average rather than rerun on all folds
  # dt_wide <- dcast(dt_results_folds, cp_cpr_risk_indu_cd ~ fold) # looks OK
  dt_temp <- dt_results_folds[fold %in% foldsWithTarget,
                              list(score = mean(score, na.rm = TRUE),
                                   nmiss = sum(is.na(score))),
                              by = highCardVar]
  
  sum(dt_temp$nmiss)
  max(dt_temp$nmiss)
  dt_temp[, nmiss := NULL]
  
  # split dt_results_folds into folds with and without target
  # merge mean result onto fold without target 
  # put back together
  dt_results_foldsWithTarget <- dt_results_folds[fold %in% foldsWithTarget]
  dt_results_foldsWithout <- dt_results_folds[!(fold %in% foldsWithTarget)]
  
  dt_results_foldsWithout[, score := NULL]
  dt_results_foldsWithout <- 
    merge(dt_results_foldsWithout,
          dt_temp,
          by = highCardVar)
  
  dt_results_folds <- rbind(dt_results_foldsWithTarget,
                            dt_results_foldsWithout)
  rm(dt_results_foldsWithTarget, dt_results_foldsWithout)
  
  dt_results_allData[, score := NULL]
  dt_results_allData <- 
    merge(dt_results_allData,
          dt_temp,
          by = highCardVar)
  
  setnames(dt_results_folds, 'score', paste0(highCardVar, '_hccv'))
  
  return(dt_results_folds)
  
}