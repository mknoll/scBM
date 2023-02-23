#Preprocessing for scRNA seq
library(dplyr)
library(Seurat)
library(patchwork)
library(Matrix)
library(ggplot2)
library(readr)
library(tidyverse)

#Loading dataset
E_MTAB_9435 <- ReadMtx(mtx = "C:/Users/dhari/Documents/GBM/E-MTAB-9435.aggregated_filtered_counts.mtx",
                       cells = "C:/Users/dhari/Documents/GBM/E-MTAB-9435.aggregated_filtered_counts.mtx_cols",
                       features = "C:/Users/dhari/Documents/GBM/E-MTAB-9435.aggregated_filtered_counts.mtx_rows" )

#Raw preprocessing based on sum of cell counts
library(scran)
library(scuttle)

qcstats <- perCellQCMetrics(E_MTAB_9435)
qcstats
qcfiler <- quickPerCellQC(qcstats)
qcfiler
GBM_a <- E_MTAB_9435[,!qcfiler$discard]
summary(qcfiler$discard)

#Creating seurat object
GBM_1 <- CreateSeuratObject(counts = GBM_a, project = "Glioblastoma", min.features = 200, min.cells = 5)
GBM_1

#Preprocessing and visualization 
GBM_1[["percent.mt"]] <- PercentageFeatureSet(GBM_1, pattern = "^MT-")
GBM_1[["log10Genes.per.UMI"]] <- log10(GBM_1$nFeature_RNA) / log10(GBM_1$nCount_RNA)
tail(GBM_1@meta.data, 5)

VlnPlot(GBM_1, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)
nCount_max <- quantile(GBM_1$nCount_RNA, probs = 0.95)
nFeature_max <- quantile(GBM_1$nFeature_RNA, probs = 0.95)

# Visualize the correlation between genes detected and number of UMIs and determine whether strong presence of cells with low numbers of genes/UMIs
GBM_1@meta.data %>% 
  ggplot(aes(x=nCount_RNA, y=nFeature_RNA, color=percent.mt)) + 
  geom_point() + 
  scale_colour_gradient(low = "gray90", high = "black") + 
  scale_x_log10() + 
  scale_y_log10() + 
  theme_classic() +
  geom_vline(xintercept = nCount_max) +
  geom_hline(yintercept = nFeature_max) +
  geom_vline(xintercept = 500) +
  geom_hline(yintercept = 250) +
  facet_wrap(~orig.ident)

#Featureplot
plot1 <- FeatureScatter(GBM_1, feature1 = "nCount_RNA", feature2 = "percent.mt")
plot2 <- FeatureScatter(GBM_1, feature1 = "nCount_RNA", feature2 = "nFeature_RNA")
plot1 + plot2

#Filtering of cells based on desired parameters- Cell-level filtering.

GBM_1<- subset(GBM_1, subset = (nFeature_RNA >= 250) & 
                 (nFeature_RNA <= nFeature_max) &
                 (nCount_RNA >= 500) & 
                 (nCount_RNA <= nCount_max) &
                 (percent.mt < 10) &
                 (log10Genes.per.UMI > 0.8))
