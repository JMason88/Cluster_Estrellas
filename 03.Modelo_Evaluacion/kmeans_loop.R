library(cluster)

kmeans_loop <- function(x, data, k = 2) {
  
  sse_p_pers <- array()
  sil_pers <- array()
  kit <- k
  for(i in 1:kit){
    # Cálculo de PAM:
    personal_kmeans <- kmeans(x, i+1, iter.max = 25)
    # Determinar el ID del medoide que le corresponde a cada registro:
    pers_centers <- personal_kmeans$centers[personal_kmeans$cluster]
    # Cálculo de SSEs: construir un vector que registre las distancias entre
    # cada objeto y su correspondiente medoide elevadas al cuadrado, y luego
    # calcular su suma. Almacenar cada SSE en un vector.
    sse_p_pers[i] <- sum(as.matrix(x)[cbind(row.names(data), pers_centers)]^2)
    # Almacenar cada valor de silhouette global
    sil_kmeans_final <- silhouette(personal_kmeans$cluster, dist(tyc1[,-c(1,10)], method = "euclidian"))
    
    sil_pers[i] <- sil_kmeans_final[,3]/length(sil_kmeans_final[,3])
  }
  
  return(list(SSE = sse_p_pers, SIL = sil_pers))  
}
