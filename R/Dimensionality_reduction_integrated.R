# Standard workflow after data integration
# Dimensionality reduction and clustering

dimensionality_reduction <- function(x){
  x <- ScaleData(x, verbose = TRUE)
  x <- RunPCA(x, npcs = 100 , verbose = FALSE)
  x <- RunUMAP(x, features = features)

}

reduced_obj <- lapply(data.combined, dimensionality_reduction)

DimPlot(reduced_obj, dims = c(1,2), reduction = "pca")
DimPlot(reduced_obj, reduction = "umap")
