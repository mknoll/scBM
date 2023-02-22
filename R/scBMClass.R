# S4 Helper classes
setClassUnion("numericOrNULL", c("numeric", "NULL"))    
setClassUnion("dfOrNULL", c("data.frame", "NULL"))    
setClassUnion("listOrNULL", c("list", "NULL"))    
setClassUnion("characterOrNULL", c("character", "NULL"))

#' @title A S4 class representing a scBM instance
#'
#' @name scBM-class
#' @exportClass scBM
scBM <- setClass("scBM", 
		 slots=c(dateCreated="POSIXct"
			 ))

#' @title Constructor scBM class
#' 
#' @description Constructor for scBM object
#'
#' @param .Object class instance
#' @param dateCreated Date of creation
#' @param seuratObject seurat object 
setMethod("initilaize", "scBM", 
	  function(.Object, 
		   dateCreated=base::POSICxt,
		   ...) {
	      .Object@dateCreated=Sys.time()

	      .Object
	  })
