coeff_sim <- vector()

for (i in c(1:29)){
  v <- get_incidence[[i]]
  df <- data.frame(t = c(1:15), inc = v)
  mult_lm <- lm(log(inc) ~ t, data = df)
  lm_coef <- coef(mult_lm)
  coeff_sim <- c(coeff_sim, lm_coef[2])
}

gen_time <- 5.4
print(exp(coeff_sim*gen_time))
R_0 <- exp(coeff_sim*gen_time)

#results
R_0v <- c(4.149475,4.644061,2.985611,3.067750,7.883687,3.788906,3.930972,3.638931,2.927877,3.932834,6.031600,6.591765,3.547018,6.136693,3.894102,3.716047,4.085398,2.668536,3.630486,9.093520,3.217887,4.412453,4.890080,9.365082,3.130587,4.712540,5.005118,1.923416,4.244152)
