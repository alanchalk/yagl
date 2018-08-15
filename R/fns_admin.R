# fns_admin.R

# Purpose: Various functions for admin such as loading packages

# Functions in this script:

# fn_pkgInstallLoad
# 


#--------------------------------------------------------------------------------
#' Install and load packages
#' 
#' Check if packages are installed and if not then installs them.  See
#' # http://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages
#' 
#' @export
#' 
#' @param x a vector of packages
#' 
fn_pkgInstallLoad <- function(x)
{ 
    
    # Install package if needed
    if (!(x %in% rownames(installed.packages())))
    {
        install.packages(x, dep = TRUE)
    }
    
    # Silently load package
    suppressPackageStartupMessages(
        require(x, character.only = TRUE, quietly = TRUE,
                warn.conflicts = FALSE)
    )
} 


#--------------------------------------------------------------------------------
#' Update an existing .RData store
#' 
#' Update an existing .RData store
#' source: https://stackoverflow.com/questions/11813096/updating-an-existing-rdata-file 
#' @export
#' 
#' @param ... new or updated objects to add
#' @param dt_ the data table from which to create the model matrix
#' @param file the .RData file to update
#'   
#' @return a reduced size earth object
#' 
fn_resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var_names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var_names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var_names)), file = file)
}
