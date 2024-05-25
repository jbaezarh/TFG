
# Librerías ---------------------------------------------------------------
# Se cargan las librerías que se usarán en esta sección

library(terra) # Raster data
library(sf) # Vector data
library(mapSpain) # Polígonos de las regiones de España
library(tidyverse) # Manipulación de datos
library(lubridate) # Manipulación de fechas


# CRS de referencia -------------------------------------------------------
# Será el CRS que se use en todo el proyecto

pend <- rast("data_raw/topograficas/pendiente.tif")
crs_reference = crs(pend)
rm(pend)




# Polígono de Andalucía ---------------------------------------------------
Andalucia <- esp_get_ccaa(ccaa = "Andalucía") # Se obtiene el polígono de la comunidad autónoma de Andalucía
andalucia_proj <- st_transform(Andalucia,crs_reference) # Se transforma al sistema de referencia usado en el 

#* *HACER CROP con el mapa de andalucia en los incendios y en la zona de suelo forestal para evitar problemas*

# area_monte es el área donde se generarán las muestras negativas.
# Dado que no hay un mapa que indique claramente cuales son las zonas que se consideran "monte en Andalucía"
# Y dado que que los polígonos de incendios también cubren zonas agrícolas y urbanas (aunque menores en número
# que las zonas forestales), voy a considerar monte toda Andalucía, sin distinción. 

area_monte <- andalucia_proj


# Generación de la muestra ------------------------------------------------

# Generación de la muestra estratificando por mes de forma que la proporción de 
# observaciones positivas y negativas por mes (en todo el periodo) sea la misma

# 1089 incencios correctamente registrados entre 2002 y 2022


#     Tamaño muestral -----------------------------------------------------

n_in=10 # Número de puntos a muestrear dentro de cada poligono
n_out=1089*10 # Número de muestras negativas



#     Generación aleatoria de fechas para las muestras negativas ----------

# Se generan fechas aleatorias para las muestras negativas entre 2002 y 2022 siguiendo la misma
# distribución de probabilidad mensual que en los incendios observados:

incendios = NULL

for (year in 2002:2022) {
  incendios = rbind(incendios, 
                    st_read(paste0("./data_raw/incendios_2000-2022/incendios_",year,".shp")) %>% 
                      select("FECHA_INIC" = matches("(?i)^FECHA_INIC$|^fecha_inic.$"))) 
}


incendios_mes = incendios %>% 
  mutate(FECHA_INIC = ymd(FECHA_INIC),.keep="unused") %>% 
  filter(!is.na(FECHA_INIC)) %>% 
  filter(year(FECHA_INIC)<=2022,year(FECHA_INIC)>=2002) %>% 
  st_drop_geometry() %>% 
  mutate(MES = month(month(FECHA_INIC))) %>% 
  count(MES) 


possible_dates = tibble (date = seq(as.Date('2002/01/01'), as.Date('2022/12/31'), by="day")) %>% 
  mutate(MES = month(date)) %>% 
  left_join(incendios_mes,
            join_by(MES)) 

set.seed(12345) # Fijamos una semilla

dates = sample(possible_dates$date, n_out,replace = T,prob = possible_dates$n) # Estas serán las fechas de las muestras negativas

rm(incendios, possible_dates) # Se borran para liberar memoria



#     Selección de localizaciones aleatorias ------------------------------

# Se genera la muestra de observaciones:

points_in = NULL
points_out = NULL

for (year in 2002:2022) {
  
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

save(sample,file=paste0("salidas_intermedias/sample_strat_",Sys.Date(),".RData"))





