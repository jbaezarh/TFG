
# Librerías ---------------------------------------------------------------
# Se cargan las librerías que se usarán

library(terra) # Raster data
library(sf) # Vector data
library(mapSpain) # Polígonos de las regiones de España
library(tidyverse) # Manipulación de datos

# CRS de referencia -------------------------------------------------------
# Será el CRS que se use en todo el proyecto

pend <- rast("data_raw/topograficas/pendiente.tif")
crs_reference = crs(pend)
rm(pend)


# Polígono de Andalucía ---------------------------------------------------

Andalucia <- esp_get_ccaa(ccaa = "Andalucía")
andalucia_proj <- st_transform(Andalucia,crs_reference)

#* *HACER CROP con el mapa de andalucia en los incendios y en la zona de suelo forestal para evitar problemas*

# area_monte es el área donde se generarán las muestras negativas.
# Dado que no hay un mapa que indique claramente cuales son las zonas que se consideran "monte en Andalucía"
# Y dado que que los polígonos de incendios también cubren zonas agrícolas y urbanas (aunque menores en número
# que las zonas forestales), voy a considerar monte toda Andalucía, sin distinción. 

area_monte <- andalucia_proj


# Generación de la muestra ------------------------------------------------

# total 1174: 41 NA's y 1 erróneo (fecha) -> 1132 incencios correctamente registrados

#     Tamaño muestral -----------------------------------------------------

n_in=10 # Número de puntos a muestrear dentro de cada poligono
n_out=1132*10 # Número de muestras negativas

set.seed(12345) # Fijamos una semilla

#     Generación aleatoria de fechas para las muestras negativas ----------
# Fechas aleatorias para generar la muestra negativa

dates <- sample(seq(as.Date('2000/01/01'), as.Date('2022/12/31'), by="day"), n_out,replace = T)


#     Selección de localizaciones aleatorias ------------------------------

points_in = NULL
points_out = NULL

for (year in 2000:2022) {
  
  cat("YEAR ", year," : -------------------------------------\n")
  cat("  Generando muestras positivas...\n")
  incendios <- st_read(paste0("./data_raw/incendios_2000-2022/incendios_",year,".shp"),quiet=T) |> 
    st_transform(crs = crs_reference) |> 
    rename_with(.fn=tolower) |> 
    mutate(fecha_inic=ymd(fecha_inic),geometry,.keep="none")
  
  
  ## Generación de puntos positivos
  
  for (i in 1:nrow(incendios)) {
    point_in_sfc <- st_sample(incendios[i,],size=n_in) # Se generan n_i puntos dentro de cada incendio
    point_in_attr <- data.frame(fire = rep(1,n_in),date = rep(incendios[i,]$fecha_inic,n_in))
    point_in <- st_sf(point_in_attr,geometry= point_in_sfc)
    
    if (is.null(points_in)) {
      points_in <- point_in 
    } else {
      points_in <- points_in |> 
        add_row(point_in) 
    }
  }
  
  ## Generación muestra negativa 
  cat("  Generando muestras negativas...\n")
  # ---> Nota: los puntos se generan en area_monte
  
  dates_year <- dates[year(dates) == year]
  locations = NULL
  
  for (day in dates_year) {
    incendios_day = filter(incendios,fecha_inic>=day-3 & fecha_inic<=day+3)
    if (nrow(incendios_day)==0){ # Si no ha habido incendios en una franja de 6 días en Andalucía
      if (is.null(locations)) {
        locations = st_sample(area_monte,size=1)
      } else {
        locations = c(locations, st_sample(area_monte,size=1))
      }
    } else { # Si ha habido algún incendio en una franja de 6 días en Andalucía (3 antes o 3 después)
      repeat {
        possible_location = st_sample(area_monte,size=1)
        if (!st_is_within_distance(possible_location,st_union(incendios_day), dist = 15000, sparse = FALSE)) {
          possible_location = st_sample(area_monte,size=1)
          if (is.null(locations)) {
            locations = possible_location
            break
          } else {
            locations = c(locations, possible_location)
            break
          }
        }
      }
    }
  }
  
  
  points_out_attr <- data.frame(fire = rep(0,length(dates_year)),date = dates_year)
  
  if (is.null(points_out)) {
    points_out <- st_sf(points_out_attr,geometry= locations)
  } else {
    points_out <- points_out |> 
      add_row(st_sf(points_out_attr,geometry= locations)) 
  }
  
}



sample <- rbind(points_in,points_out) # Muestra generada

# Comprobación ------------------------------------------------------------

summary(sample) # Hay una fecha de un incendio errónea
max(sample$date,na.rm=T) # "2033-08-15"
sum(sample$date==max(sample$date,na.rm=T),na.rm=T) # Aparece 3 veces, es un incendio que tiene mal la fecha
# Para evitar errores vamos a eliminar las 3 ocurrencias de esta fecha:

# Corrección --------------------------------------------------------------
# Se eliminan las observaciones con fecha de incendio errónea que se han detectado

sample <- sample[-which(sample$date==max(sample$date,na.rm=T)),]
summary(sample)

# Almacenamiento de resultados --------------------------------------------

save(sample,file=paste0("salidas_intermedias/sample_",Sys.Date(),".RData"))




# NOTA Area Monte: --------------------------------------------------------

# Alternativa a considerar toda Andalucía:

# # Zonas de "monte" en Andalucia (en realidad solo he considerado las zonas forestales y zonas húmedas)
# load("data_cleaning/suelo_forestal_and.RData") # suelo_forestal_and
# suelo_forestal_and <- st_transform(suelo_forestal_and,crs_reference) |># Uso una simplificación porque es un archivo muy pesado
#   st_simplify(dTolerance = 100) # Tolerancia de 100m
# # > as.numeric(object.size(suelo_forestal_and_simp))/as.numeric(object.size(suelo_forestal_and))
# # [1] 0.04667928
# # Se reduce 20 veces el tamaño del archivo y solo estoy considerando una tolerancia de 100m que para
# # mi trabajo no supone ninguna diferencia
# # area_monte <- suelo_forestal_and
