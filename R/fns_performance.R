# fns_performance.R

# Purpose: Functions for calculating performance


# Functions in this script:

# fn_y_log_y (not exported)
# fn_deviance_gaussian (not exported)
# fn_deviance_binomial (not exported)
# fn_deviance_poisson (not exported)
# fn_deviance_gamma (not exported)
# fn_deviance
# fn_gini
# fn_normalizedGini

# No extra packages used in this script
# devtools::use_package("")



fn_y_log_y <- function(y, mu){
  ifelse(y > 0, y * log(y/mu), 0)
}

# Functions below are taken from glm.fit by running a glm and inspecting the formula returned eg
# clotting <- data.frame(
#   u = c(5,10,15,20,30,40,60,80,100),
#   lot1 = c(118,58,42,35,27,25,21,19,18),
#   lot2 = c(69,35,26,21,18,16,13,12,12))
# clotting.1 <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
# clotting.1$family$dev.resids
# see https://stats.stackexchange.com/questions/52513/summary-of-residuals-in-r

# binomial function comes from C here
# https://github.com/wch/r-source/blob/master/src/library/stats/src/family.c

fn_deviance_gaussian <- function(y, mu, wt) {
  wt * ((y - mu)^2)
}

fn_deviance_binomial <- function(y, mu, wt) {
  2 * wt * (fn_y_log_y(y, mu) + fn_y_log_y(1 - y, 1 - mu))
}


fn_deviance_poisson <- function(y, mu, wt) {
  r <- mu * wt
  p <- which(y > 0)
  r[p] <- (wt * (y * log(y/mu) - (y - mu)))[p]
  2 * r
}

fn_deviance_gamma <- function(y, mu, wt) {
  -2 * wt * (log(ifelse(y == 0, 1, y/mu)) - (y - mu)/mu)
}

#' Calculate proportion of deviance explained
#'
#' The functions for calculating the deviance are taken from the glm.fit function and
#' the relevant c code.  Results have not been tested against glm output.  For poisson family
#' y and mu vectors need to be claims counts and not frequencies
#'
#' @export
#'
#' @param y numeric vector of actual values
#' @param mu A vector of predictions
#' @param wt weights (for example, exposure)
#' @param family family (one of; gaussian, binomial, poisson, gamma)
#'
#' @return deviance, null deviance, proportion explained
#'
fn_deviance <-

  function(y, mu, wt = NULL, family = NULL){

    # modData = data.frame(actual=actual,
    #                      pred = pred,
    #                      exposure = exposure)
    #
    # glm_fit = glm(actual ~ offset(log(pred)),
    #               data = modData,
    #               family = poisson)
    #
    # null_fit = glm(actual ~ offset(log(exposure)),
    #                data = modData,
    #                family = poisson)
    #
    # glm_deviance = summary(glm_fit)$null.deviance
    # null_deviance = summary(null_fit)$null.deviance

    if (!(length(mu) %in% c(1, length(y)))) stop("mu must be length 1 or same length as y")

    if (!is.null(wt) & !(length(wt) %in% c(1, length(y)))) stop("wt must be length 1 or same length as y")

    if(is.null(family)) stop("You need to input the family (one of: gaussian, poisson, binomial)")
    if(!(family %in% c("gaussian", "poisson", "binomial")) ) stop("Family must be one of: gaussian, poisson, binomial)")

    if (is.null(wt)) wt <- rep(1, length(y))
    wtdmu <- rep(sum(wt * y)/sum(wt), length(y))

    if(family == "gaussian") {
      fn_deviance_family <- fn_deviance_gaussian
    } else if (family == "poisson"){
      fn_deviance_family <- fn_deviance_poisson
    } else if (family == "binomial"){
      fn_deviance_family <- fn_deviance_binomial
    } else if (family == "gamma"){
      fn_deviance_family <- fn_deviance_gamma
    }

    glm_deviance <- sum(fn_deviance_family(y, mu, wt))
    null_deviance <- sum(fn_deviance_family(y, wtdmu, wt))

    D2 <- 1 - glm_deviance / null_deviance

    return_object = list(deviance_model = glm_deviance,
                         deviance_null = null_deviance,
                         proportion_explained = D2)

    return(return_object)
  }


#---------------------------------------------------------------------

#' Calculate gini
#'
#' Calculate gini, can allow for weights.  First calculates AUC using trapezium rule
#'     and then sets gini = 2 * (0.5 - auc) - where auc was defined based on
#'     predictions sorted in increasing value.  Actual and predictions are expected
#'     to be numeric and neither of them are limited to being 0-1.
#'
#' @export
#'
#' @param actual vector of actual responses
#' @param pred vector of predicted responses
#' @param weight
#'
fn_gini <-

  function(actual, pred, weight = NULL){

    if(sum(is.na(actual)) > 0) stop("Some actuals are NA")
    if(sum(is.na(pred)) > 0) stop("Some predictions are NA")
    if(!(class(actual) %in% c("numeric", "integer"))) stop("actuals must be numeric")
    if(!(class(pred) %in% c("numeric", "integer"))) stop("predictions must be numeric")
    if (length(actual) !=  length(pred)) stop("actual and predicted need to be equal lengths!")

    # get the number of observations
    n <- length(actual)
    if(is.null(weight)) weight <- rep(1, n)

    # put inputs into a data.frame
    df <- data.frame(actual = actual,
                    pred = pred,
                    weight = weight)

    # order data.frame by increasing predicted values
    df <- df[order(df$pred, decreasing = FALSE),]

    # calculate the cumulative proportion of the actuals
    df$cumprop_actual <- cumsum(df$actual) / sum(df$actual)

    # calculate the cumulative proportion of the weights
    df$cumprop_weight <- cumsum(df$weight)/sum(df$weight)

    # trapezium rule
    auc = sum((df$cumprop_weight[-1] - df$cumprop_weight[-n]) *
                (df$cumprop_actual[-1] + df$cumprop_actual[-n])
              )/2

    # calculate gini coefficient, area between the curve and diagonal
    # gini is 2 * auc - 1 unless it has been defined unusually above
    gini = 2 * (0.5 - auc)

    return(gini)
  }


#---------------------------------------------------------------------

# https://www.kaggle.com/wiki/RCodeForGini

#' Calculate normalised gini
#'
#' Calculate normalised gini
#'
#' @export

#' @param actual vector of actual responses
#' @param pred vector of predicted responses
#'
#' @return normalised gini
#'
fn_normalizedGini <- function(actual, pred) {

  gini <- function(a, p) {
    if (length(a) !=  length(p)) stop("actual and predicted need to be equal lengths!")
    temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
    temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
    population.delta <- 1 / length(a)
    total.losses <- sum(a)
    null.losses <- rep(population.delta, length(a))
    accum.losses <- temp.df$actual / total.losses
    gini.sum <- cumsum(accum.losses - null.losses)
    sum(gini.sum) / length(a)
  }
  gini(actual, pred) / gini(actual, actual)
}

>>>>>>> local initial commit
