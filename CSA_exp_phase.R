library(readxl)
pop_data <- read_excel(".../csa_pop.xlsx", skip = 3)
pop_CSA <- pop_data$`2019`[-c(1,8, 19, 22)]

ind_CSA <- c(15, 108, 57, 59, 81, 51, 16, 1, 191, 1, 17, 179, 58, 63, 142, 11, 1, 159, 1, 72, 29, 1, 1, 1, 19, 12, 185, 111, 1)

get_incidence <-list()
for (i in 1:29){
  get_incidence[[i]] <- c()
}

fold_names <- c("Atlanta", "Boston", "Brownsvi", "Cape Cor", "Chicago", "Columbia", "Denver", "Detroit", "Greenville", "Hartford", "Houston", "Indianap", "Jackson", "Lafayett", "Las Vega", "Los Ange", "Miami", "Minneapolis", "New_Orleans", "New_York", "North", "Orlando", "Philadelphia", "Phoenix", "SanAntonio", "Shreveport", "StLouis", "Tucson", "Washington")

for (k in c(1:29)){
  fold <- fold_names[k]
  datetime <- datetime_fold[k]
  i <- 1 
  j <- ind_CSA[k]
  data <- read.csv(paste(".../Output/", fold, "/Phase_", i, "_Repeat_", j, ".csv", sep = ""), skip = 1)
  data_inc <- data$Incidence
  get_incidence[[k]] <- data_inc/pop_CSA[k]
}

plot(c(0:14), get_incidence[[1]], ylim = c(0,0.00021), type = "l", xlab = "Days", ylab = "Cases/population")

for (i in c(2:29)){
  points(c(0:14), get_incidence[[i]], type = "l")
}
