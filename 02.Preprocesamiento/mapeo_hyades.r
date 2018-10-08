## Función para llamar al Dataframe final de Hipparcos ##

mapeo_hyades <- function(){
  setwd(getwd())
  hip <- read_excel("../00.Datos_y_TP/hyades_source.xlsx", sheet = 2)
  seleccion <- read_csv("../00.Datos_y_TP/Hyades_seleccion.csv")
  
  hip <- hip %>%
    mutate(Symbad_Hyades = (.$HIP %in% seleccion$cercanaHip))

return(hip)}

