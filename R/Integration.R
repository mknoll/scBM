#Post preprocessing for integrated data
# To identify feature anchors for data integration

t1 <- c(preprocessed.obj)

Normalization <- function(x){
  x <- NormalizeData(x)
  x <- FindVariableFeatures(x, selection.method = "vst", nfeatures = 2000)
}
Normalized_list <- lapply(t1, Normalization)

# select features that are repeatedly variable across datasets for integration
features <- SelectIntegrationFeatures(object.list = Normalized_list)

#Integration
anchors <- FindIntegrationAnchors(object.list = Normalized_list, anchor.features = features)
data.combined <- IntegrateData(anchorset = anchors)

