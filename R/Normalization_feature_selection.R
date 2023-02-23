#Normalization and Feature Selection
GOI <- c("TGFB1", "TGFB2", "TGFB3", "TGFBR1", "TGFBR2", "TGFBR3", "SMAD1", "SMAD2", "SMAD3", "SMAD4", "SMAD7", 
         "PIK3CA", "TNF", "TRAF6", "TRAF4", "NFKB1", "MAP3K7", "MAPK14", "MAPK1", "MAPK3", "MAPK8")
#If only ensembl ID is available
GOI_Ensembl <- c("ENSG00000228978", "ENSG00000223952", "ENSG00000175387",  "ENSG00000100030", "ENSG00000119699", "ENSG00000101665", " ENSG00000141646",  "ENSG00000069702", "ENSG00000163513", "ENSG00000102882", "ENSG00000092969", "ENSG00000107643", "ENSG00000105329", "ENSG00000170365", "ENSG00000166949", "ENSG00000109320", "ENSG00000106799", "ENSG00000121879", "ENSG00000232810", "ENSG00000076604", "ENSG00000135341", "ENSG00000112062", "ENSG00000175104")

#Normalization 
GBM_1 <- NormalizeData(GBM_1 , normalization.method = "LogNormalize", scale.factor = 10000)
GBM_1  <- FindVariableFeatures(GBM_1, selection.method = "vst", nfeatures = 2000)
all_vg_1 <- VariableFeatures(GBM_1)
all_genes <- rownames(GBM_1)
top_10 <- head(VariableFeatures(GBM_1), 10)
top_10
capture.output(top_10, file = "Top_10_genes.txt")

#Visualization of highly variable genes
plot1 <- VariableFeaturePlot(GBM_1)
plot2 <- LabelPoints(plot = plot1, points = top_10, repel = TRUE)
plot1 + plot2

common <- intersect(all_vg_1, GOI_Ensembl)
common

plot3 <- LabelPoints(plot = plot1, points = common, repel = TRUE)
plot1 + plot3

#Scaling data
GBM_1 <- ScaleData(GBM_1, features = all_genes)
GBM_1
head(GBM_1@meta.data,5)

#Checking for HK genes expression
HK <- FetchData(GBM_1, vars = c("GAPDH", "ACTB"))
expr <- colSums(HK)
HK_expr <- rowSums(HK)
which(HK_expr == 0)

