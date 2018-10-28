#  TYCO EN EL CLOUD

rm(list = ls())

list.of.packages = c("tidyr", "tidyverse", "ggplot2", "readr", "cluster", "factoextra", "NbClust", "ggsci")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# librerias
library(tidyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(cluster)
library(factoextra)
library(NbClust)
library(ggsci)

# SETUP
mPath = 'C:/Users/Fabio/Documents/Maestria/CienciaYTech/tp1/Cluster_Estrellas/'
debuggg = FALSE

switch (Sys.info()[['sysname']],
        Windows = {
          filter = TRUE
          directory.codigoR  =  paste0(mPath, "03.Modelo_Evaluacion/")
          directory.include  =  paste0(mPath, "codigoR/include/")
          directory.work     =  paste0(mPath, "work/")
          directory.plan     =  paste0(mPath, "plan/")
          directory.datasets =  paste0(mPath, "02.Preprocesamiento/")
        },
        Linux   = {
          filter = FALSE
          directory.codigoR  =  "~/cloud/cloud1/codigoR/xgbfz/"
          directory.include  =  "~/cloud/cloud1/codigoR/include/"
          directory.work     =  "~/cloud/cloud1/work/"
          directory.plan     =  "~/cloud/cloud1/plan/"
          directory.datasets =  "~/cloud/cloud1/datasets/"
        })

PAM_loop <- function(x, data, k = 2) {
  nombresCol = c()
  clusterings = c()  
  sse_p_pers <- array()
  sil_pers <- array()
  kit <- k
  for(i in 1:kit){
    # Cálculo de PAM:
    personal_pam <- pam(x, i+1, diss = T, do.swap = FALSE)
    # Determinar el ID del medoide que le corresponde a cada registro:
    pers_meds <- personal_pam$medoids[personal_pam$clustering]
    # Cálculo de SSEs: construir un vector que registre las distancias entre
    # cada objeto y su correspondiente medoide elevadas al cuadrado, y luego
    # calcular su suma. Almacenar cada SSE en un vector.
    sse_p_pers[i] <- sum(as.matrix(x)[cbind(row.names(data), pers_meds)]^2)
    # Almacenar cada valor de silhouette global
    sil_pers[i] <- personal_pam$silinfo$avg.width
    clusterings = cbind(clusterings, "cluster" = personal_pam$silinfo$widths[,1])
    nombresCol = c(nombresCol, paste0("cluster", i))
    clusterings = cbind(clusterings, "neighbor" = personal_pam$silinfo$widths[,2])
    nombresCol = c(nombresCol, paste0("neighbor", i))
    clusterings = cbind(clusterings, "sil_width" = personal_pam$silinfo$widths[,3])
    nombresCol = c(nombresCol, paste0("sil_width", i))
  }
  colnames(clusterings) = nombresCol
  return(list(SSE = sse_p_pers, SIL = sil_pers, clusterings = clusterings))  
}

karchivo_salida_base = "tyco_output_report"
karchivo_salida = paste(karchivo_salida_base, "kmeans", "log", sep = ".")
karchivo_salida_pam = paste(karchivo_salida_base, "pam", "log", sep = ".")
set.seed(123)

setwd(directory.datasets)
tyc1 = read.csv('tyc_version1.csv')
if (filter | debuggg) {
  tyc1 = tyc1[c(4000:7000), ]
}

setwd(directory.work)
time_start = Sys.time()
cat(
  "======================================",
  "\n",
  paste("Inicio:", time_start),
  "\n\n",
  sep = "\t",
  file = karchivo_salida,
  fill = FALSE,
  append = FALSE
)

diss_euclid_tyc = scale(tyc1[, -c(1:3, 10)]) %>% dist(., method = "euclidian")

PAM_TYC_Euclid = PAM_loop(x = diss_euclid_tyc, data = tyc1[,-c(1:3, 10)], k = 19)

save(PAM_TYC_Euclid, file = karchivo_salida_pam)
# rm(PAM_TYC_Euclid)
# load(file = karchivo_salida_pam)

SSE = PAM_TYC_Euclid[[1]]
Sil = PAM_TYC_Euclid[[2]]
Clusterings = as.data.frame(PAM_TYC_Euclid$clusterings)


time_stop = Sys.time()
cat(
  "======================================",
  "\n",
  paste("Final:", time_stop),
  "\n\n",
  sep = "\t",
  file = karchivo_salida,
  fill = FALSE,
  append = TRUE
)

#limpio la memoria
rm( list=ls() )
gc()

#salgo del R sin grabar el gigante entorno
quit( save="no" )


####
