library(zoo)

CSA_names_data <- list("Atlanta", "Boston_W", "Brownsvi", "Cape Cor", "Chicago", "Columbia", "Denver", "Detroit", "Greenville", "Hartford", "Houston", "Indianapolis", "Jackson", "Lafayette", "Las", "Los", "Miami", "Minneapolis", "New_Orleans", "New_York", "North", "Orlando", "Philadelphia", "Phoenix", "SanAntonio", "Shreveport", "StLouis", "Tucson", "Washington")
CSA_names <- list("Atlanta", "Boston-W", "Brownsville-H", "Cape Cor", "Chicago", "Columbia-O", "Denver","Detroit", "Greenville-S", "Hartford", "Houston", "Indianapolis", "Jackson-Vicksburg", "Lafayette-Opelousas", "Las Vegas", "Los Ang", "Miami-F", "Minneapolis", "New Orleans", "New York", "North Port", "Orlando", "Philadelphia", "Phoenix", "SanAntonio", "Shreveport", "St. Louis", "Tucson", "Washington-B")

list_data <- list()

for (i in 1:length(CSA_names_data)){
  list_data[[i]] <- readRDS(paste(".../Curve_CSA/data_", CSA_names_data[i], ".rds", sep = ""))
}

list_data_new <- list_data

plot(list_data_new[[1]]$mean, type = "l", col = "blue", ylim = c(0,1), ylab = "Curve flattening index", xlab = "Day")
plot(list_data_new[[2]]$mean, type = "l", col = "red", ylim = c(0,1), ylab = "Curve flattening index", xlab = "Day")

for (i in 1:length(clusters_row$clusters_row)){
  if (clusters_row$clusters_row[i]==1){
    points(list_data_new[[i]]$mean, type = "l", col = "blue")
  }else{
    if (clusters_row$clusters_row[i]==2){
    points(list_data_new[[i]]$mean, type = "l", col = "red")
    }
  }
}
