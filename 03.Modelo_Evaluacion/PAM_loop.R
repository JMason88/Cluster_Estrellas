
PAM_loop <- function(x, data, k = 2) {
  
  sse_p_pers <- array()
  sil_pers <- array()
  kit <- k
  for(i in 1:kit){
    # Cálculo de PAM:
    personal_pam <- pam(x, i+1, diss = T)
    # Determinar el ID del medoide que le corresponde a cada registro:
    pers_meds <- personal_pam$medoids[personal_pam$clustering]
    # Cálculo de SSEs: construir un vector que registre las distancias entre
    # cada objeto y su correspondiente medoide elevadas al cuadrado, y luego
    # calcular su suma. Almacenar cada SSE en un vector.
    sse_p_pers[i] <- sum(as.matrix(x)[cbind(row.names(data), pers_meds)]^2)
    # Almacenar cada valor de silhouette global
    sil_pers[i] <- personal_pam$silinfo$avg.width
  }
  
  return(list(sse_p_pers, sil_pers))  
}


