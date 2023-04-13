# Standard workflow after data integration
# Dimensionality reduction and clustering

data.combined <- readRDS("/integrated.rds")

dimensionality_reduction <- function(x){
  x <- ScaleData(x, verbose = TRUE)
  x <- RunPCA(x, npcs = 100 , verbose = FALSE)

}

reduced_obj <- lapply(data.combined, dimensionality_reduction)
DimPlot(reduced_obj, dims = c(1,2), reduction = "pca")

# Determine percent of variation associated with each PC
pct <- reduced_obj[["pca"]]@stdev / sum(reduced_obj[["pca"]]@stdev) * 100

# Calculate cumulative percents for each PC
cumu <- cumsum(pct)

# Determine which PC exhibits cumulative percent greater than 90% and % variation associated with the PC as less than 5
co1 <- which(cumu > 90 & pct < 5)[1]
co1

# Determine the difference between variation of PC and subsequent PC
co2 <- sort(which((pct[1:length(pct) - 1] - pct[2:length(pct)]) > 0.1), decreasing = T)[1] + 1

# last point where change of % of variation is more than 0.1%.
co2
pcs <- min(co1, co2)
print(pcs)

# Create a dataframe with values
plot_df <- data.frame(pct = pct, 
                      cumu = cumu, 
                      rank = 1:length(pct))

# Elbow plot to visualize 
p1 <- ggplot(plot_df, aes(cumu, pct, label = rank, color = rank > pcs)) + 
  geom_text() + 
  geom_vline(xintercept = 90, color = "grey") + 
  geom_hline(yintercept = min(pct[pct > 5]), color = "grey") +
  theme_bw()
grid.text("Elbowplot to select number of dimensions", .5, 1, just = "top", gp=gpar(cex=1.5))

p1

saveRDS(reduced.obj, "/dataset.rds")

