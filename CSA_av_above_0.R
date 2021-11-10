setwd(".../Data_CSA/")
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.table, header = FALSE, sep = ",", stringsAsFactors=FALSE)

names <- list.files(path = ".")

fp_new <- vector()

for (i in 1:32){

    rt <-  data.frame(t(myfiles[[i]]))
    v <- rt$X1
    lv <- length(v) - 7
    la <- vector()
    for (j in 1:lv){
      f <- j + 6
      m <- mean(v[j:f])
      la <- c(la,m)
    }
    
    index <- min(which(round(la)>0))
    fp_new <- c(fp_new, index)
}
    