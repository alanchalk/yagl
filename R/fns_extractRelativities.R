# fns_extractRelativities.R

# Purpose: For extracting relativities along the hinges etc

# Functions in this script:
# fn_hinge
# fn_linearSplines
# fn_relativitiesModelCoefToDataTable

# devtools::use_package()


#--------------------------------------------------------------------------------
#' Create basis functions
#' 
#' Function to create hinge.  If the knot is passed in with a negative sign
#' a hinge from that point and below is created.  Otherwise from that point
#' and above
#' 
#' @param x the input data vector
#' @param knotPlusMinus the knot point with +/- in front as described 
#'        above
#' 
#' @return the basis function (vector)
#' 
fn_hinge <- function(x, knotPlusMinus) { 
  if (knotPlusMinus <=0){
    basis <- pmax(0, abs(knotPlusMinus)-x)  
  } else {
    basis <- pmax(0, x-knotPlusMinus)
  }     
  basis[is.na(basis)] <- 0
  basis
}


#--------------------------------------------------------------------------------
#' A function to create linear spline basis functions
#'
#' @export
#'
#' @param underlying the underlying data (vector expected but
#'        can be dataframe or table with one column)
#' @param varName the variable name of the data in the vector
#' @param knots the points at which we require the hinges to be
#' @param lessOrMore 'less' means the hinge will go upwards for values less than 
#'         the knot and be zero above.  'more' is vice verca.
#'
#' @return at data table of basis functions
#' 
fn_linearSplines <- function(underlying, 
                             varName, 
                             knots, 
                             lessOrMore){
  call <- match.call()
  if (class(underlying)[1] %in% c("data.table", "data.frame")) {
    underlying <- underlying[[1]]}
  
  # create the basis functions (linear splines)
  minusPlus <- ifelse(lessOrMore == 'less', '-', '+')
  minusPlusKnots <- as.numeric(paste0(minusPlus, knots))
  dt_basis <- 
    as.data.table(
      matrix(rep(underlying, length(knots)), ncol = length(knots))
    )
  
  # create the column names
  colnames(dt_basis) <- paste0('ls_', varName, '_', knots, '_', lessOrMore)
  idx <- 0
  for (j in colnames(dt_basis)){ #j = "V1"
    idx <- idx + 1
    dt_basis[, (j) := fn_hinge(.SD, minusPlusKnots[idx]), .SDcols = j]
  }
  dt_basis
}


#--------------------------------------------------------------------------------
#' Purpose: Create a datatable of coefficients from a glm model
#' 
#' @export
#' 
#' @param model the name of the model object
#' @param modelName the variable name for the coefficient in the datatable output
#' @param s the value for lambda if model is glmnet
#' 
#' @return a data.table of coefficients
#' 
fn_relativitiesModelCoefToDataTable <- 
  function(model, modelName, s = "lambda.1se"){

    if ("cv.glmnet" %in% class(model)){
      mat_ <- as.matrix(coef(model, s = s))
      dt_ <- as.data.table(mat_)
      dt_[, varLevel := rownames(mat_)]
      setnames(dt_, "1", modelName)
      dt_[, (modelName) := .SD, .SDcols = modelName]
      return(dt_)
    } else if ("glm" %in% class(model)){
      mat_ <- as.matrix(coef(model))
      dt_ <- as.data.table(mat_)
      dt_[, varLevel := rownames(mat_)]
      setnames(dt_, "V1", modelName)
      dt_[, (modelName) := .SD, .SDcols = modelName]
      dt_
    } else if ("H2ORegressionModel" %in% class(model)){
      dt_ <- as.data.table(model@model$coefficients_table)
      setnames(dt_, "names", "varLevel") 
      dt_[, standardized_coefficients := NULL]
      setnames(dt_, "coefficients", modelName)
      dt_[, (modelName) := .SD, .SDcols = modelName]
      dt_
    }
  }


#--------------------------------------------------------------------------------
#' Function to split h(..) into something readable
#' 
#' Function to split h(..) into something readable
#' Three levels of interactions are allowed
#' Manual hacks in here for Sabre data (VarXXCat, _m (missing),
#' and _red variables
#' 
#' @export
#' 
#' @param x a vector of variable names and h type names 
#' @param cNames a list of all possible column names (from dt_all)
#' 
#' @return a dataframe of varname, knots and coefficients
#' 
fn_split_h_syntax <- function(x, cNames = NULL){  
  # x = dt_coef$varLevel[173]
  # cNames = colnames(dt_all)
  # originally if there were no h's then I just returned straight back
  # no idea why I did this since it could have been just categorical 
  # variables.  But this fell over big time when I used my own splines
  # without h prefix
  #if (length(grep("h\\(", x)) == 0) {  
  #    df_coefs <- data.frame(varName1 = x)
  #    return(df_coefs)
  #} else {
  n_coefs <- length(x)
  if (n_coefs > 0){
    varsUsed <- gsub("h\\(", '', x)
    varsUsed <- gsub("\\)", '', varsUsed)
    varsUsed <- strsplit(varsUsed, '\\*')
    
    df_coefs <- data.frame(varName1 =  rep(NA, length(x)))
    df_coefs$varName2 <- NA
    df_coefs$varName3 <- NA
    df_coefs$knot1 <- NA
    df_coefs$knot2 <- NA
    df_coefs$knot3 <- NA
    df_coefs$lessOrMore1 <- NA
    df_coefs$lessOrMore2 <- NA
    df_coefs$lessOrMore3 <- NA
    
    # go through the vars used, convert to numbers (knot point)
    # or variable name and add to df_coeffs
    # if not an earth string ie not starting with h(
    # then hardcode the VarxxCAT pattern
    library(stringr, quietly = TRUE)
    for (idx_i in 1:length(varsUsed)){ 
      print(idx_i)
      # idx_i = 1
      if (length(grep('h\\(', x[idx_i])) > 0){
        for (idx_j in 1:length(varsUsed[[idx_i]])){  # 
          varNumber <- paste0('varName', idx_j, sep = "")
          knotNumber <- paste0('knot', idx_j, sep = "")
          lessOrMoreNumber <- paste0('lessOrMore', idx_j, sep = "")
          feature <- varsUsed[[idx_i]][idx_j]
          # note string split using - does not work when there are two -'s
          idx_minus <- str_locate_all(pattern = '-', feature)
          idx_minus <- idx_minus[[1]][idx_minus[[1]][, 1] != 1,][1]
          if (!is.na(idx_minus)) {
            feature <- c(substr(feature, 1, idx_minus-1),
                         substr(feature, idx_minus + 1, nchar(feature))
            )
          }
          idx_lt_gt <- 0
          for (idx_k in 1:length(feature)){ 
            variableOrKnot <- type.convert(feature[idx_k], as.is = TRUE)
            if (is.na(variableOrKnot)) continue
            if (is.character(variableOrKnot)){
              idx_lt_gt <- idx_lt_gt + 1
              df_coefs[idx_i, varNumber] <- variableOrKnot
            } 
            # if it is a number, need to the number and wether the basis fn is non zero less or more than the number 
            if (is.numeric(variableOrKnot)){
              idx_lt_gt <- idx_lt_gt + 1
              df_coefs[idx_i, knotNumber] <- round(variableOrKnot,1)
              if (idx_lt_gt ==1) df_coefs[idx_i, lessOrMoreNumber] <- "less" else df_coefs[idx_i, lessOrMoreNumber] <- "more"
            }
          }    
        } # idx = 1; x[1] = 'Var29_mY'
      } else if (substr(x[idx_i], 1, 3) == 'Var'){
        # This else was for a specific client where variable names started with Var
        df_coefs$varName1[idx_i] <- substr(x[idx_i], 1, 5)
        df_coefs$knot1[idx_i] <- substr(x[idx_i], 6, nchar(x[idx_i]))
        # correct for the _m variables which do not fit this pattern
        if (substr(df_coefs$knot1[idx_i], 1, 2) == '_m'){
          df_coefs$knot1[idx_i] <- gsub('_m', '', df_coefs$knot1[idx_i])
          df_coefs$varName1[idx_i] <- paste0(df_coefs$varName1[idx_i], '_m')
        } # correct for the _red variables
        if (substr(df_coefs$knot1[idx_i], 1, 4) == '_red'){
          df_coefs$knot1[idx_i] <- gsub('_red', '', df_coefs$knot1[idx_i])
          df_coefs$varName1[idx_i] <- paste0(df_coefs$varName1[idx_i], '_red')
        }
      } else if (x[idx_i] != "(Intercept)"){
        # This else is also specific to another client where
        # there is no identifier - should have made all var names
        # end with some standard string but did not
        # so import colnames to this function and do in a round-about way
        matches <- names(
          unlist(
            sapply(cNames, function(idx_cName) grep(idx_cName, x[idx_i]))
          )
        )
        best_match <- matches[which.max(sapply(matches, nchar))]
        df_coefs$varName1[idx_i] <- best_match
        df_coefs$knot1[idx_i] <- gsub(df_coefs$varName1[idx_i], "", x[idx_i])
      }
      
    }
  }
  
  return(df_coefs)
  #}
}
