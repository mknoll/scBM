
dataset <- readRDS("/dataset.rds")

#pcs value is saved from dimensional reduction script

#Clustering
dataset <- FindNeighbors(dataset, reduction = "pca", dims = 1:pcs)
dataset <-FindClusters(dataset, resolution = 0.5)
head(Idents(dataset), 5)

dataset <- RunUMAP(dataset, reduction = "pca", dims = 1:pcs)
DimPlot(dataset, reduction = "umap", split.by = "orig.ident")

#Finding number of cells in each cluster from each dataset
pt <- table(Idents(GBM_integrated), GBM_integrated@meta.data$orig.ident)
pt

#Extracting cluster markers
dataset.markers <- FindAllMarkers(dataset, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
markers %>%
  group_by(cluster) %>%
  slice_max(n = 15, order_by = avg_log2FC)

#DoHeatmap() generates an expression heatmap for given cells and features. Plotting is done for 20 markers here or all if less than 20
.markers %>%
  group_by(cluster) %>%
  top_n(n = 20, wt = avg_log2FC) -> top10
DoHeatmap(GBM_1, features = top20$gene) + NoLegend()


#Cluster annotation is done manually and then reassigned to the clusters
#Given annotations are for a sample dataset
new.cluster.ids <- c("Macrophages/Microglial cells", "Microglial cells", "Astrocytes", "Dendritic cells", "Endothelial cells", "Macrophages (proliferating)", "Macrophages", "Conventional Dendritic cell 2b", "Pericytes",
                     " ", "Conventional Dendritic cell 2b", "T-cells", "Neutrophils", "B-cells")
names(new.cluster.ids) <- levels(dataset)
GBM_integrated <- RenameIdents(dataset, new.cluster.ids)
DimPlot(dataset, reduction = "umap", label = TRUE, pt.size = 0.5) + NoLegend()

# Visualization
VlnPlot(dataset, features = GOI)
FeaturePlot(dataset, features = GOI)
DotPlot(dataset, features = GOI)