#fns_splines.R

# Purpose: Various functions for splines


# Functions in this script:

# fn_hinge
# fn_linearSplines
# fn_cubicSplines_ns
# fn_cubicSplines_bs
# fn_cubicSplines_wg


# Packages used
# devtools::use_package("")


#--------------------------------------------------------------------------------
#' Create one linear spline basis function
#'
#'
#' Function to a create hinge.  If the knot is passed in with a negative sign
#' a hinge from that point and below is created.  Otherwise from that point
#' and above
#'
#' @export
#'
#' @param x a vector of data from which to create the hinge
#' @param knot  the value at which to make the hinge / knot
#' @param lessOrMore 'less' or 'more'.  Determines if hinge is positive above (more)
#'     or below ('less') the knot
#'
#' @return the value of the hinge at each point of the original vector of data
#'
fn_hinge <- function(x, knot, lessOrMore) {
  if(!(lessOrMore %in% c('less', 'more'))) stop('fn_hinge: lessOrMore must be either less or more')
  if(length(lessOrMore) > 1) stop('fn_hinge: In this function, lessOrMore must be length one only')
  if (lessOrMore == "less"){
    basis <- pmax(0, knot - x)
  } else {
    basis <- pmax(0, x - knot)
  }
  basis[is.na(basis)] <- 0
  basis
}


#--------------------------------------------------------------------------------
#' A function to create linear spline basis functions at given knots
#'
#' A function to create linear spline basis functions at given knots
#'
#' @export
#'
#' @param underlying an underlying vector of data
#' @param varName the variable name of the data in the vector
#' @param knots the points at which we require the hinges to be
#' @param lessOrMore 'less' means the hinge will go upwards for values less than
#'    the knot and be zero above.  'more' is vice verca
#'
#' @return a data frame with as many columns as needed for the basis
#'
fn_linearSplines <- function(underlying, varName, knots, lessOrMore){
  if(!(length(lessOrMore) %in% c(1, length(knots)))){
    stop('fn_linearSplines: length of lessOrMore must be 1 or same length as knots')
  }
  if (length(lessOrMore) == 1) lessOrMore <- rep(lessOrMore, length(knots))

  # If the underlying is passed not as a vector but as a dataframe or table,
  # we first convert it to a vector
  if (class(underlying)[1] %in% c("data.table", "data.frame")) {
    underlying <- underlying[[1]]}

  # Set up empty matrix to store the basis
  dt_basis <-
    as.data.table(
      matrix(rep(underlying, length(knots)), ncol = length(knots))
    )

  # create the column names
  colnames(dt_basis) <- paste0('ls_', varName, '_', knots, '_', lessOrMore)

  idx <- 0
  for (j in colnames(dt_basis)){
    idx <- idx + 1
    dt_basis[, (j) := fn_hinge(.SD, knots[idx], lessOrMore[idx]), .SDcols = j]
  }
  dt_basis
}


#--------------------------------------------------------------------------------
#' Create a basis for a natural cubic spline
#'
#' Create a basis for a natural cubic spline.  Based on the ns function which uses
#' the qr decomposition which gets very slow when there is lots of data.
#'
#' @export
#'
#' @param x the vector of data
#' @param knots the inner knots
#' @param varName the prefix to be used for the column names
#' @param Boundary.knots the boundary knots.  Can be NULL in which cases the
#'     0.001 and 0.999 percentiles are used.
#'
#' @return a list: bx_ the basis as a sparse matrix; cnames the column names
#'
fn_cubicSplines_ns <- function(x, knots, varName, Boundary.knots = NULL){
  # x = x[ , varName, with = FALSE][[1]]

  if (is.null(Boundary.knots)){
    Boundary.knots <- unname(quantile(x, probs = c(0.001, 0.999)))
  }
  range.knots <- range(knots)
  Boundary.knots[1] <- min(Boundary.knots[1], range.knots[1])
  Boundary.knots[2] <- max(Boundary.knots[2], range.knots[2])
  knots <- setdiff(knots, Boundary.knots)
  intercept = FALSE

  Aknots <- sort(c(rep(Boundary.knots, 4L), knots))
  basis <-
    splineDesign(Aknots,
                 x = pmax(Boundary.knots[1], pmin(Boundary.knots[2], x)),
                 ord = 4,
                 sparse = TRUE)

  const <- splineDesign(Aknots, Boundary.knots, 4, c(2, 2), sparse = TRUE)
  if (!intercept) {
    const <- const[, -1, drop = FALSE]
    basis <- basis[, -1, drop = FALSE]
  }
  qr.const <- qr(t(const))

  bx_ <- Matrix((t(qr.qty(qr.const, t(basis))))[, -(1L:2L),
                                                drop = FALSE])

  cnames <- paste0(varName, '_', 1:dim(bx_)[2])
  ret_ <- list(bx_ = bx_, cnames = cnames)

  return(ret_)

}


#--------------------------------------------------------------------------------
#' Create a basis for a cubic spline
#'
#' Create a basis for a cubic spline.
#'
#' @export
#'
#' @param x the vector of data
#' @param knots the inner knots
#' @param varName the prefix to be used for the column names
#' @param Boundary.knots the boundary knots.  Can be NULL in which cases the
#'     0.001 and 0.999 percentiles are used.
#'
#' @return a list: bx_ the basis as a sparse matrix; cnames the column names
#'
fn_cubicSplines_bs <- function(x, knots, varName, Boundary.knots = NULL){
  # varName = 'Var27'
  # x = dt_all[1:5000 ,varName, with = FALSE][[1]]
  # x = x[ ,varName, with = FALSE][[1]]
  # knots = knots
  # knots = unlist(df_$x[df_$nk == varName])
  if (is.null(Boundary.knots)){
    Boundary.knots <- unname(quantile(x, probs = c(0.001, 0.999)))
  }
  range.knots <- range(knots)
  Boundary.knots[1] <- min(Boundary.knots[1], range.knots[1])
  Boundary.knots[2] <- max(Boundary.knots[2], range.knots[2])
  knots <- sort(setdiff(knots, Boundary.knots))

  # ord 4 is degree 3 ie cubic
  Aknots <- sort(c(rep(Boundary.knots, 4), knots))

  bx_ <-
    splineDesign(
      knots = Aknots,
      x = pmax(Boundary.knots[1], pmin(Boundary.knots[2], x)),
      ord = 4,
      sparse = TRUE)
  # dim(bx_)
  cnames <- paste0(varName, '_', 1:dim(bx_)[2])
  ret_ <- list(bx_ = bx_, cnames = cnames)

  return(ret_)
}


#--------------------------------------------------------------------------------
#' create basis for a cubic spline manually (Wood pp126 & 127, Wahba and Gu)
#'
#' create basis for a cubic spline manually (Wood pp126 & 127, Wahba and Gu)
#'
fn_cubicSplines_wg <- function(x, knots){
  # x = x[ ,varName, with = FALSE][[1]]
  # knots = c(14,32)
  Boundary.knots <- unname(quantile(x, probs = c(0.001, 0.999)))
  range.knots <- range(knots)
  Boundary.knots[1] <- min(Boundary.knots[1], range.knots[1])
  Boundary.knots[2] <- max(Boundary.knots[2], range.knots[2])
  knots <- sort(union(knots, Boundary.knots))
  n_knots <- length(knots)
  # These 2 functions are from Wood p127
  fn_rk <- function(x, z){
    ((z - 1/2)^2 - 1/12) * ((x - 1/2)^2 - 1/12) / 4 -
      ((abs(x - z) - 1/2)^4 - 1/2*(abs(x - z) - 1/2)^2 + 7/240) / 24
  }
  fn_spl_X <- function(x, xk){
    q <- length(xk) + 2
    n <- length(x)
    X <- Matrix(1, n, q)
    X[,2] <- x
    X[, 3:q] <- outer(x, xk, FUN = fn_rk())
    return(X)
  }
  bx_ <- bs(pmax(Boundary.knots[1],
                 pmin(Boundary.knots[2], x)),
            knots = knots)
  class(bx_) <- 'matrix'
  return(Matrix(bx_))
}


>>>>>>> local initial commit
