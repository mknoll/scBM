#Preprocessing for scRNA seq
library(dplyr)
library(Seurat)
library(patchwork)
library(Matrix)
library(ggplot2)
library(readr)
library(tidyverse)


t1 <- readRDS("/filename.rds")

#Preprocessing and visualization 
t1[["percent.mt"]] <- PercentageFeatureSet(t1, pattern = "^MT-")
t1[["log10Genes.per.UMI"]] <- log10(t1$nFeature_RNA) / log10(t1$nCount_RNA)
VlnPlot(t1, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)

t1 <- c(t1)
preprocessing = function(x) {
  median_ncount <- median(x$nCount_RNA)
  median_nfeatures <- median(x$nFeature_RNA)
  median_mt <- median(x$percent.mt)
  
  mad_ncounts <- mad(x$nCount_RNA, center = median_ncount)
  mad_nfeatures <- mad(x$nFeature_RNA, center = median_nfeatures)
  mad_mean_mt <- mad(x$percent.mt, center = median_mt)
  
  total_ncounts <- median_ncount+mad_ncounts
  total_nfeatures <- median_nfeatures+mad_nfeatures
  total_mt <- median_mt+mad_mean_mt
  
  x@meta.data %>% 
    ggplot(aes(x=nCount_RNA, y=nFeature_RNA, color=percent.mt)) + 
    geom_point() + 
    scale_colour_gradient(low = "gray90", high = "black") + 
    scale_x_log10() + 
    scale_y_log10() + 
    theme_classic() +
    geom_vline(xintercept = total_ncounts) +
    geom_hline(yintercept = total_nfeatures) +
    geom_vline(xintercept = 500) +
    geom_hline(yintercept = 250) +
    facet_wrap(~orig.ident)
  
  x<- subset(x, subset = (nFeature_RNA >= 250) & 
                  (nFeature_RNA <= total_nfeatures) &
                  (nCount_RNA >= 500) & 
                  (nCount_RNA <= total_ncounts) &
                  (percent.mt < total_mt) &
                  (log10Genes.per.UMI > 0.8))
 
}

preprocessed.obj <- lapply(t1, preprocessing)
preprocessed.obj
