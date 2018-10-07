# Busqueda de estrellas idénticas en diferentes catalogos

# Carga de archivos en R
#
# Carga del catalogo Hipparcos
hip <- read.csv("hip.csv", header=T)
# Una lista de estrellas Hyades en el catalogo Symbad:
symbad <- read.csv("symbad.csv", header=T, stringsAsFactors=F)

# Los estrellas en el siguiente archivo son las unicas estrellas que sabemos que son 
# Hyades y ademas conocemos su ID en Hipparcos
idcruz <- read.csv("id_cruzada.csv", header=F, stringsAsFactors=F)
# Nombres de variables para los archivos de IDs cruzadas
names(idcruz) <- c("HD", "HIP")

# Los identificadores tienen un espacio en blanco al final
symbad$identifier <- sub("\\W$", "", symbad$identifier)

# Agregamos una variable al dataframe Symbad indicando cuales 
# de las Hyades tienen ID en Hipparcos
symbad$idHip <- symbad$identifier %in% idcruz$HD

# Preparacion de la grilla espacial
# numero de intervalos (igual sobre AR y DE)
nint <- 50
ra.min <- min(hip$RA_J2000)
ra.max <- max(hip$RA_J2000)
de.min <- min(hip$DE_J2000)
de.max <- max(hip$DE_J2000)
ra.dif <- ra.max - ra.min
de.dif <- de.max - de.min
ra.sp <- ra.dif/(nint-1)
de.sp <- de.dif/(nint-1)

# Calculo de la ubicacion de las estrellas de Hipparcos en la grilla
# Nuestro vecindario estará delimitado por un cuadrado 3x3
vecx <- c(-1,0,1)
vecy <- c(-1,0,1)
ind.estrella <- data.frame(matrix(nrow=nrow(hip), ncol=2))
names(ind.estrella) <- c("rec","dec")
ind.estrella$rec <- floor((hip$RA_J2000 - ra.min)/ra.sp) + 1 
ind.estrella$dec <- floor((hip$DE_J2000 - de.min)/de.sp) + 1 

# Tambien calculamos para las Hyades de Symbad su ubicación en la grilla
symbad$rac <- floor((symbad$RA_J2000 - ra.min)/ra.sp) + 1 
symbad$dec <- floor((symbad$DE_J2000 - de.min)/de.sp) + 1 

# Dos funciones auxiliares

obtener.vecinas <- function(consulta){
# Esta funcion busca las estrellas mas proximas a una estrella del 
# listado Symbad  
  # primero busca cuales son las celdas a consultar
  exstar <- consulta$rac
  eystar <- consulta$dec
  exstar.vec <- (exstar + vecx) [exstar + vecx > 0]
  eystar.vec <- (eystar + vecy) [eystar + vecy > 0]
  star.vec <- expand.grid(rec=exstar.vec, dec=eystar.vec)
  # luego se extrae un dataframe con las estrellas que estan en la zona de consulta
  vecinas <- hip[ind.estrella$rec == star.vec$rec[1] & ind.estrella$dec == star.vec$dec[1],]
  for(j in 2:nrow(star.vec)) {
    vecinas <- rbind(vecinas, hip[ind.estrella$rec == star.vec$rec[j] & ind.estrella$dec == star.vec$dec[j],] )  
  }
  return(vecinas)
  # retorna el dataframe
}

mas.cercana <- function(consulta, vecinas){
# Esta funcion a partir de la conuslta Symbad y del listado de vecinas
# busca la mas cercana  
  mindist <- sqrt(sum((consulta[c(6,8)] - vecinas[1,c(2,3)])^2))
  masvecina <- vecinas[1,]
  for(i in 2:nrow(vecinas)){
    nvd = sqrt(sum((consulta[c(6,8)] - vecinas[i,c(2,3)])^2))
    if(nvd < mindist) {
      mindist = nvd
      masvecina <- vecinas[i,]
    }
  }
  # la funcion retorna la distancia y los datos de la estrella mas cercana
  # una mejora de esta funcion seria generar una alarma si dos estrellas estan igual de
  # cerca a la consulta
  return(list(mas.vecina=masvecina, dist.min=mindist))
}

# Obtenemos las estrellas de Hipparcos mas cercanas a las de Symbad y su distancia
symbad.dists <- array()
symbad.mas.vecina <- data.frame("HIP"=NA, "RA_J2000"=NA, "DE_J2000"=NA, "Plx"=NA, 
                                "pmRA"=NA, "pmDE"=NA, "Vmag"=NA, "B_V"=NA)

for(j in 1:nrow(symbad)){
  vecinas <- obtener.vecinas(symbad[j,])
  posibles.iguales <- mas.cercana(symbad[j,], vecinas )
  symbad.dists[j] <- posibles.iguales$dist.min
  symbad.mas.vecina[j,] <- posibles.iguales$mas.vecina
}

# El vector de distancias
symbad.dists
# El dataframe de estrellas más cercanas en Hipparcos a las de Symbad.
symbad.mas.vecina

# Como se ven los datasets superpuestos
plot(symbad$RA_J2000, symbad$DE_J2000, pch=20, col="blue")
points(symbad.mas.vecina$RA_J2000, symbad.mas.vecina$DE_J2000, pch=21, col="red")

# Podemos agregar la informacion obtenida a un dataframe Symbad aumentado
symbad.aumentado <- data.frame(symbad, dist=symbad.dists, cercanaHip=symbad.mas.vecina$HIP )

# Finalmente agregamos al dataframe aumentado el ID Hipparcos de la identificacion cruzada
symbad.aumentado$idcruzHip <- NA
for(i in 1:nrow(symbad.aumentado)){
  if(symbad.aumentado$identifier[i] %in% idcruz$HD) {
    symbad.aumentado$idcruzHip[i] <- idcruz$HIP[idcruz$HD==symbad.aumentado$identifier[i]]
  }
}
symbad.aumentado$idcruzHip <- as.numeric(sub("HIP ", "", symbad.aumentado$idcruzHip))

