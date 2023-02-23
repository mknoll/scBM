#Clustering
GBM_1 <- FindNeighbors(GBM_1, reduction = "pca", dims = 1:50)
GBM_1 <-FindClusters(GBM_1, resolution = 0.1)
head(Idents(GBM_1), 5)
CalculateSilhouette(GBM_1)
df <- as.data.frame(CalculateSilhouette(GBM_1), row.names = FALSE)
sum(df$width <0)
PCAPlot(GBM_1)
UMAPPlot(GBM_1)

#Extracting cluster markers
GBM_1.markers <- FindAllMarkers(GBM_1, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
GBM_1.markers %>%
  group_by(cluster) %>%
  slice_max(n = 2, order_by = avg_log2FC)

#DoHeatmap() generates an expression heatmap for given cells and features. Plotting is done for 20 markers here or all if less than 20
GBM_1.markers %>%
  group_by(cluster) %>%
  top_n(n = 20, wt = avg_log2FC) -> top10
DoHeatmap(GBM_1, features = top20$gene) + NoLegend()

# Visualization
VlnPlot(GBM_1, features = GOI)
FeaturePlot(GBM_1, features = GOI)

#Cluster annotation is done manually and then reassigned to the clusters
