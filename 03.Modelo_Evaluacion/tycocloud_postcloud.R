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

karchivo_salida_base = "tyco_output_report"
karchivo_salida = paste(karchivo_salida_base, "kmeans", "log", sep = ".")
karchivo_salida_pam = paste(karchivo_salida_base, "pam", "log", sep = ".")

rm(PAM_TYC_Euclid)
setwd(directory.work)
load(file = karchivo_salida_pam)

SSE = PAM_TYC_Euclid[[1]]
Sil = PAM_TYC_Euclid[[2]]
Clusterings = as.data.frame(PAM_TYC_Euclid$clusterings)

setwd(directory.datasets)
tyc1 = read.csv('tyc_version1.csv')
tyc1$Symbad_Hyades = !is.na(tyc1$Symbad_Hyades)

Clusterings = cbind(Symbad_Hyades = tyc1$Symbad_Hyades, Clusterings)

table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster1)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster2)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster3)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster4)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster5)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster6)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster7)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster8)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster9)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster10)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster11)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster12)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster13)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster14)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster15)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster16)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster17)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster18)
table(Hyades = Clusterings$Symbad_Hyades, Cluster = Clusterings$cluster19)

setwd(directory.work)
jpeg(paste("Silhouette_PAM_scaled", "jpg", sep = "."))
plot(2:(20),
     Sil,
     type = "b",
     xlab = "k",
     sub = "Silhouette")
dev.off()

jpeg(paste("SSE_PAM_scaled", "jpg", sep = "."))
plot(2:(20),
     SSE,
     type = "b",
     xlab = "k",
     sub = "SSE")
dev.off()
