setwd(".../Data_CSA/")
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.table, header = FALSE, sep = ",", stringsAsFactors=FALSE)

names <- list.files(path = ".")

b3 <- vector()

for (i in 1:32){
if (i %in% c(7,18,21)){
  b3 <- c(b3, NA)
}else{
vs <- vector()
vd <- vector()
exp_coeff <- vector()
print(i)
rt <-  data.frame(t(myfiles[[i]]))

v <- rt$X1
lv <- length(v) - 7
la <- vector()
for (j in 1:lv){
  f <- j + 6
  m <- mean(v[j:f])
  la <- c(la,m)
}

end <- fp_new[i] + 15
for (j in fp_new[i]:end){
b <- j
e <- b + 14

df <- data.frame(t = c(1:15), inc = round(la[b:e]))

mult_lm <- lm(log(inc) ~ t, data = df)
coef(mult_lm)

lm_coef <- coef(mult_lm)

# make the plot
plot(df$t, df$inc, main = j)
lines(df$t, exp(lm_coef[1])*exp(lm_coef[2]*df$t), col = "dodgerblue", lwd = 2)
exp_coeff <- c(exp_coeff, lm_coef[2])
s <- sum(abs(mult_lm$residuals))
vd <- c(vd,j)
vs <- c(vs,s)
}

#plot(exp_coeff, main = i)

index <- which (vs == min(vs))

if (max (exp_coeff) >= 0.25){
while(exp_coeff[index]<0.25){
  vs <- vs[-index]
  exp_coeff <- exp_coeff[-index]
  vd <- vd[-index]
  index <- which (vs == min(vs))
}}else{
  while(exp_coeff[index]<0.2){
    vs <- vs[-index]
    exp_coeff <- exp_coeff[-index]
    vd <- vd[-index]
    index <- which (vs == min(vs))
  }
}

b3 <- c(b3, vd[index])
}
}

#results
b3 <- c(41, 49, 56, 50, 40, 43, NA, 41, 50, 50, 49, 48, 47, 46, 52, 43, 44, NA, 48, 44,
        NA, 45, 42, 48, 47, 46, 46, 50, 48, 47, 49, 40)
