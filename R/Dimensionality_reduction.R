#Dimensionality reduction
GBM_1 <- RunPCA(GBM_1, features = all_vg_1)
GBM_1@reductions$pca@cell.embeddings

#PCA visualization
print(GBM_1[["pca"]], dims = 1:10, nfeatures = 5)
VizDimLoadings(GBM_1, reduction = "pca" )
DimPlot(GBM_1, reduction = "pca", dims = c(1,2))
DimHeatmap(GBM_1, dims = 1, cells = 500, balanced = TRUE)
DimHeatmap(GBM_1, dims = 1:15, cells = 500, balanced = TRUE)

#Choosing number of dimensions for finding neighbour elements
GBM_1 <- JackStraw(GBM_1, dims = 50, reduction = "pca")
GBM_1 <- ScoreJackStraw(GBM_1, dims = 1:50)

JackStrawPlot(GBM_1, dims = 1:50)
ElbowPlot(GBM_1, ndims = 50, reduction = "pca")

#Run UMAP
GBM_1 <- RunUMAP(GBM_1, dims = NULL, features = all_vg_1, reduction.name = "umap", min.dist = 0.01)
DimPlot(GBM_1, reduction = "umap", group.by = "individual")