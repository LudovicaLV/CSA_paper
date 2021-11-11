library(zoo)
library(pheatmap)

CSA_names_data <- list("Atlanta", "Boston_W", "Brownsvi", "Cape Cor", "Chicago", "Columbia", "Denver", "Detroit", "Greenville", "Hartford", "Houston", "Indianapolis", "Jackson", "Lafayette", "Las", "Los", "Miami", "Minneapolis", "New_Orleans", "New_York", "North", "Orlando", "Philadelphia", "Phoenix", "SanAntonio", "Shreveport", "StLouis", "Tucson", "Washington")
CSA_names_heat <- list("Atlanta", "Boston", "Brownsville", "Cape Coral", "Chicago", "Columbia", "Denver","Detroit", "Greenville", "Hartford", "Houston", "Indianapolis", "Jackson", "Lafayette", "Las Vegas", "Los Angeles", "Miami", "Minneapolis", "New Orleans", "New York", "North Port", "Orlando", "Philadelphia", "Phoenix", "San Antonio", "Shreveport", "St. Louis", "Tucson", "Washington")

list_data <- list()

for (i in 1:length(CSA_names_data)){
  list_data[[i]] <- readRDS(paste(".../Curve_CSA/data_", CSA_names_data[i], ".rds", sep = ""))
}

#looking at area under the curve, for 20 time points after the first value > 0
area20 <- list()

for (i in 1:20){
  area20[[i]] <- vector()
}

area2 <- rep(0, length(CSA_names_data))

#ss <- vector()

for (j in 1:20){
  for (i in 1:length(CSA_names_data)){
    data <- list_data[[i]]
    x <- 1:length(data$mean)
    y <- data$mean
    start <- min(which(y > 0))
    #print(start)
    start <- start - 1
    #ss <- c(ss, start)
    k <- j-1
    id <- c(start+k, start+j)
    AUC <- sum(diff(x[id])*rollmean(y[id],2))
    area2[i] <- area2[i] + AUC
  }
  area20[[j]] <- area2
}

area20_df <- as.data.frame(area20, col.names = c(1:20))
row.names(area20_df) <- unlist(CSA_names_heat)

X20_scale <- scale(area20_df$X20)
X20_s <- unlist(X20_scale[,1])

R_scale <- (scale(R_0v))
R_s <- unlist(R_scale[,1])

area20_df <- data.frame(X20 = X20_s, R_0 = R_s)
row.names(area20_df) <- unlist(CSA_names_heat)

# dataset:
data <- as.matrix(area20_df)

# Default Heatmap
hh <- pheatmap(data,main = "CSA Heatmap",
               show_colnames = TRUE, 
               show_rownames = TRUE,
               #annotation_col = variable_labels,
               cutree_rows = 2, fontsize = 16)

clusters_row <- cutree(hh$tree_row, k = 2)
clusters_row <- as.data.frame(clusters_row)

#elbow test
library(tidyverse) 
library(cluster)   
library(factoextra) 

## elbow - k-means
set.seed(123)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(area20_df, k, nstart = 10, iter.max = 20)$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:10
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
