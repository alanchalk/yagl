# fns_admin.R

# Purpose: Various functions for admin such as loading packages

# Functions in this script:

# fn_prepareTrainingData
# fn_downsample
# fn_earthReduceSize



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


