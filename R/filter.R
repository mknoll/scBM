#' @title Filter single cell data
#' 
#' @description Applies cutoffs on single cell experiment (Seurat object)
#'
#' @param obj scBM instance
#'
#' @import Seurat
#' @export
#' 
#' @return scBM object with filtered data
filter <- function(obj, ...) {
    if (is.null(obj)) {
	warning("Empty object!")
	return(NULL)
    }

    ## TODO: Implement filters

    return(obj)
}
