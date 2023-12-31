# Q1. Install the stemHypoxia data package from Bioconductor by following the 
#     instructions here: https://bioconductor.org/packages/devel/data/experiment/html/stemHypoxia.html
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("stemHypoxia")
library(stemHypoxia)
# Q2. Load the stemHypoxia gene expression data.frame and draw a boxplot of gene expressions for all samples
#     hint: find the help page for the dataset to get you started. BUT, there is an error 
#           in the example code, how can you fix it?
data(stemHypoxia)
##Boxplot of gene expressions
boxplot(M[,-c(1,2)]) 
boxplot(M[,c(-1, -2)]) 

# Q3. Make a new column in the gene expression data.frame for the mean expression of each gene across
#     all the samples. Sort the data.frame from highest to lowest mean expression and subset the top
#     10 genes (with the highest mean expression).
#     hint: Useful functions for this task are rowMeans(), base::order()/dplyr::arrange()
rowMeans(M[, -c(1,2)]) -> mean_exp
M$rowmeans <- mean_exp

colnames(M)

M[order(M$rowmeans, decreasing = TRUE), ] %>% head(n = 10) # sorting with base R

M %>% arrange(rowmeans) %>% head(n = 10) # sorting using tidyverse
