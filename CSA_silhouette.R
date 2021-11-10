#silhouette analysis
sil <- silhouette(clusters_row$clusters_row, dist(area20_df))
fviz_silhouette(sil, label = TRUE)

