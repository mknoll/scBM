scBM <- R6Class("scBM",
		public=list(
			    seuratObject=NULL,
			    initialize=function(
						seuratObject=NA) {
				self$seuratObject <- seuratObject 
				self$greet()
			    },
			    greet=function() {
				print("Hi there")
			    }
		)
	)


