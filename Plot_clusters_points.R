#2 clusters
s_v <- vector()
f_v <- vector()

for (i in 1:29){
  if (clusters_row$clusters_row[i] == 1){
    s_v <- c(s_v, i)
  }else{
    if(clusters_row$clusters_row[i] == 2){
      f_v <- c(f_v, i)
    }
  }
}

area <- area20_df$X20
#area <- area20_df$area
R0 <- R_0v

plot(area[s_v], R0[s_v], col = "blue", xlim = c(3,17), ylim = c(1,10), pch = 19, xlab = "Area 20-day range", ylab = "R_0")
points(area[f_v], R0[f_v], col = "red", pch = 19)

#3 clusters
s_v <- vector()
f_v <- vector()
m_v <- vector()

for (i in 1:29){
  if (clusters_row$clusters_row[i] == 1){
    s_v <- c(s_v, i)
  }else{
    if(clusters_row$clusters_row[i] == 2){
      f_v <- c(f_v, i)
    }else{
      if(clusters_row$clusters_row[i] == 3){
        m_v <- c(m_v, i)
      }
    }
  }
}

area <- area20_df$X20
R0 <- area20_df$R0

plot(area[s_v], R0[s_v], col = "blue", xlim = c(3,17), ylim = c(1,10), pch = 19, xlab = "Area 20-day range", ylab = "R_0")
points(area[f_v], R0[f_v], col = "red", pch = 19)
points(area[m_v], R0[m_v], col = "purple", pch = 19)
