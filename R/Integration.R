#Post preprocessing for integrated data
# To identify feature anchors for data integration

preprocessed.obj_n <- readRDS("/preprocessed.rds")
#Prepare list of preprocessed object
#list <- c(preprocessed.obj_1, 2, ........)

#Normalization
list <- lapply(X = list, FUN = function(x) {
  x <- NormalizeData(x)
  x <- FindVariableFeatures(x, selection.method = "vst", nfeatures = 1000)
})

# select features that are repeatedly variable across datasets for integration
features <- SelectIntegrationFeatures(object.list = list, nfeatures = 1000)

#Scaling and PCA to find integration anchors
list <- lapply(X = list, FUN = function(x) {
  x <- ScaleData(x, features = features, verbose = FALSE)
  x <- RunPCA(x, features = features, verbose = FALSE)
})

#Find anchors
anchors <- FindIntegrationAnchors(object.list = list, anchor.features = features, reduction = "rpca")

#Integration
integrated_data <- IntegrateData(anchorset = anchors)

saveRDS(integrated_data, "/integrated.rds")


