library(zoo)
library(pheatmap)

CSA_names_data <- list("Atlanta", "Boston_W", "Brownsvi", "Cape Cor", "Chicago", "Columbia", "Denver", "Detroit", "Greenville", "Hartford", "Houston", "Indianapolis", "Jackson", "Lafayette", "Las", "Los", "Miami", "Minneapolis", "New_Orleans", "New_York", "North", "Orlando", "Philadelphia", "Phoenix", "SanAntonio", "Shreveport", "StLouis", "Tucson", "Washington")

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

ss <- vector()

for (j in 1:20){
  for (i in 1:length(CSA_names_data)){
    data <- list_data[[i]]
    x <- 1:length(data$mean)
    y <- data$mean
    start <- min(which(y > 0))
    start <- start - 1
    if (j == 1){
    ss <- c(ss, start)
    }
    k <- j-1
    id <- c(start+k, start+j)
    AUC <- sum(diff(x[id])*rollmean(y[id],2))
    area2[i] <- area2[i] + AUC
  }
  area20[[j]] <- area2
}

list_data_new <- list_data

b <- ss[1]
e <- b + 19

plot(list_data_new[[1]]$mean[b:e], type = "l", col = "blue", ylim = c(0,1), ylab = "Curve flattening index", xlab = "Day")

for (i in 2:26){
  if (clusters_row$clusters_row[i]==1){
    b <- ss[i]
    e <- b + 19
    points(list_data_new[[i]]$mean[b:e], type = "l", col = "blue")
  }else{
    if (clusters_row$clusters_row[i]==2){
      b <- ss[i]
      e <- b + 19
      points(list_data_new[[i]]$mean[b:e], type = "l", col = "red")
    }
  }
}

