# Cargamos las librerías
library(terra)
library(sf)
library(mapSpain)
library(tidyverse)
library(climate)
library(nasapower)
library(raster, include.only = c("rasterFromXYZ"))



# CRS de referencia
pend <- rast("data_raw/topograficas/pendiente.tif")
crs_reference = crs(pend)
rm(pend)


# Poligono de Andalucia
Andalucia <- esp_get_ccaa(ccaa = "Andalucía")
andalucia_proj <- st_transform(Andalucia,crs_reference)


# Cargamos la muestra de puntos 
load("salidas_intermedias/sample_2024-03-27.RData")

# Eliminamos las observaciones que no tienen fecha
sample <- na.omit(sample)

# # Comprobación gráfica
# plot(andalucia_proj$geometry,reset=F)
# plot(sample$geometry,add=T, col=2,pch=3,cex=0.5) # Correcto


# Variables meteorológicas ------------------------------------------------

# Tranformamos los datos a WGS84
andalucia_WGS84 <- st_transform(Andalucia,crs="WGS84")

# > st_bbox(andalucia_WGS84)
# xmin      ymin      xmax      ymax 
# -7.522943 35.936343 -1.630033 38.729133 

# Tengo que ir trabajando anualmente porque no puedo hacer consultas a la api para
# periodos mayores a 366 días
dataset = NULL

for (year in sort(unique(year(sample$date)))) {

  cat("YEAR ", year," : -------------------------------------\n")
  
  # Los puntos de cada año
  points = filter(sample,year(date)==year)  
  points_WGS84 <- st_transform(points,crs="WGS84")
  
  # Consulta a la api para obtener todo los valores del año
  daily_single_ag <- get_power(
    community = "ag",
    lonlat = c(-8,35.5,-1.5,39),
    pars = c("T2M","GWETTOP", "RH2M","WD10M","WS10M","PRECTOTCORR"),
    dates = paste0(year,c("-01-01","-12-31")),
    temporal_api = "daily")
  
  # Identificador
  daily_single_ag$clim_id <- 1:nrow(daily_single_ag)
  points$clim_id = NA # Inicializo el identificador
  
  for (day in unique(points$date)) {

    points_day = points$date==day
    
    # Seleccionar un día
    clim_day  <- filter(daily_single_ag,YYYYMMDD==day) |> 
      dplyr::select(x = LON,y = LAT,clim_id= clim_id)
    
    id_rast_day = rast(rasterFromXYZ(clim_day,crs="WGS84"))
    
    # Comprobación:
    # plot(id_rast_day)
    # points(filter(points_WGS84,date == day))
    # plot(andalucia_WGS84,add=T)
    
    points[points_day,]$clim_id <- terra::extract(id_rast_day,points_WGS84[points_day,])$clim_id
  }
  
  points <- points |> 
    left_join(select(daily_single_ag, -c(LAT,LON,DOY,YYYYMMDD)),
              by=join_by(clim_id)) |> 
    select(-clim_id)
  
  dataset = rbind(dataset,points)
}

rm(points,points_WGS84,daily_single_ag,clim_day,id_rast_day,points_day,day,year,andalucia_WGS84)

save(dataset,file = paste0("salidas_intermedias/dataset_clim",Sys.Date(),".RData"))

# Variables topográficas --------------------------------------------------
elev <- rast("data_raw/topograficas/elevacion.tif")
pend <- rast("data_raw/topograficas/pendiente.tif")
orient <- rast("data_raw/topograficas/orientacion.tif")
curv <- rast("data_raw/topograficas/curvatura.tif")

# Es necesario pasarlas a numeric para poder trabajar con ellas y extraer los valores
pend <- as.numeric(pend)
orient <- as.numeric(orient)
curv <- as.numeric(curv)

# Extraemos los valores de cada una de las capas
var_topograficas <- list(elevacion = elev,pendiente = pend,orientacion = orient,curvatura = curv)

points_topograficas <- sapply(var_topograficas,function(x) terra::extract(x,dataset))[2,] |> 
  as_tibble()

dataset <- cbind(dataset,points_topograficas)

rm(elev,pend,orient,curv,var_topograficas,points_topograficas)



# Variables antropológicas ------------------------------------------------

### Carreteras: ----
carreteras <- read_sf("data_raw/antropologicas/RedCarreteras/09_14_RedCarreteras.shp") |> 
  st_union()

dataset$dist_carretera <- st_distance(dataset,carreteras) |> 
  as.numeric()      # metres

rm(carreteras)


### Poblaciones: ----
poblaciones <- read_sf("data_raw/antropologicas/Poblaciones/07_01_Poblaciones.shp") |> 
  st_union()

dataset$dist_poblacion <- st_distance(dataset,poblaciones) |> 
  as.numeric()    # metres

rm(poblaciones)


### Linea Eléctrica: ----
linea_electrica <- read_sf("data_raw/antropologicas/LineaElectrica/10_14_LineaElectrica.shp") |> 
  st_union()

dataset$dist_electr <- st_distance(dataset,linea_electrica) |> 
  as.numeric() # metres

rm(linea_electrica)

### Ferrocarril: ----
ferrocarril <- read_sf("data_raw/antropologicas/Ferrocarril/09_21_Ferrocarril.shp") |> 
  st_union()
dataset$dist_ferocarril <- st_distance(dataset,ferrocarril) |> 
  as.numeric()

rm(ferrocarril)

### Camino / Via: ----
camino <- read_sf("data_raw/antropologicas/Camino/09_19_Camino.shp") 
viapec <- read_sf("data_raw/antropologicas/Camino/09_22_ViasPecuarias.shp") 

camino_viapec <- c(st_geometry(camino),st_geometry(viapec))
rm(camino,viapec)

camino_viapec <- st_union(camino_viapec)

dataset$dist_camino <- st_distance(dataset,camino_viapec) |> 
  as.numeric()

rm(camino_viapec)

### Sendero / Vía Verde / CarrilBici: ----
viaverde <- read_sf("data_raw/antropologicas/Sendero_ViaVerde/09_24_ViaVerde.shp") 
sendero <- read_sf("data_raw/antropologicas/sendero_ViaVerde/09_20_Sendero.shp") 
carrilbic <- read_sf("data_raw/antropologicas/sendero_ViaVerde/09_23_CarrilBici.shp")

sendero_viaverde_carrilbici <- c(st_geometry(viaverde),st_geometry(sendero),st_geometry(carrilbic)) |> 
  st_union()

dataset$dist_sendero <- st_distance(dataset,sendero_viaverde_carrilbici) |> 
  as.numeric()

rm(sendero,sendero_viaverde_carrilbici,viaverde,carrilbic)


### ENP: ----
enp1 <- read_sf("data_raw/antropologicas/ENP/11_07_Enp_FiguraProteccion.shp" )
enp2 <- read_sf("data_raw/antropologicas/ENP/11_07_Enp_RegimenProteccion.shp")


enp <- c(st_geometry(enp1),st_geometry(enp2)) |>  st_union()

# Si lo rasterizo será mucho más eficiente y puedo ponerle mucha resolución de forma
# que el error sea pequeño
enp_sf <- st_sf(enp)
enp_rast <- rasterize(enp_sf,
                      rast("data_raw/topograficas/pendiente.tif"), # Modelo
                      background = 0)

# plot(enp_rast)
dataset$enp= terra::extract(enp_rast,dataset)[,2]

rm(enp,enp1,enp2,enp_sf,enp_rast)

### Población: ----
poblacion <- read_csv2("data_raw/antropologicas/Población/poblacion_municipios.txt",
                       locale=locale(decimal_mark = ","),
                       col_select = 1:5,col_types = "ccifn") |> 
  mutate(Valor=as.integer(round(Valor))) # Por algún motivo aparecen decimales

municipios <- esp_get_munic(epsg = 4258,region = "Andalucía")
# plot(st_geometry(municipios))

municipios <- municipios |> 
  st_transform(crs_reference)

# Asociamos a cada observacion su codigo de municipio correspondiente 

num_mun = st_intersects(dataset,municipios) 

# Las eliminamos:
if (any(sapply(num_mun,function(x) length(x) == 0))) {
  cat("Eliminamos las observaciones:\n",which(sapply(num_mun,function(x) length(x) == 0)))
  dataset = dataset[-which(sapply(num_mun,function(x) length(x) == 0)),]
}

dataset$cod_municipio <- municipios[unlist(st_intersects(dataset,municipios)),]$LAU_CODE

dataset <- dataset |> 
  left_join(select(poblacion,c(-Medida)),
            join_by(cod_municipio==CODIGO_INE3,YEAR==Anual)) |> 
  rename("poblacion" = Valor,
         "municipio" = `Lugar de residencia`)
  

### Uso Suelo: ----

# UsoSuelo <- read_sf("data_raw/antropologicas/UsoSuelo/06_01_UsoSuelo.shp")
# UsoSuelo_rast <- rasterize(UsoSuelo,
#                            rast("data_raw/topograficas/pendiente.tif"), # Nos sirve de modelo
#                            field="cod_uso")

UsoSuelo_rast <- rast("data_cleaning/uso_suelo_rast.tiff")

dataset$uso_suelo = terra::extract(UsoSuelo_rast,dataset)[,2]

save(dataset,file = paste0("salidas_intermedias/dataset_clim_antr",Sys.Date(),".RData"))


# Hidrográficas -----------------------------------------------------------

# Distancia a ríos
rios <- read_sf("data_raw/hidrograficas/Rios_Espana.shp") |> 
  st_transform(st_crs(dataset)) |> 
  st_crop(xmin = 100394.4, # Esto se hace solo para no tener que considerar todo el file y que sea más manipulable
          ymin = 3976888.6,
          xmax = 690000.8,
          ymax = 4350000.0) |> 
  st_union()

dataset$dist_rios <- st_distance(dataset,rios) |> 
  as.numeric() # metres

rm(rios)


# Vegetación --------------------------------------------------------------

dataset$NDVI = NA

for (YEAR in 2000:2022) {
  for (MONTH in 1:12) {
    MM = str_pad(MONTH,2,"left",pad = "0")
    YY = substr(as.character(YEAR),3,4)
    # if (as.numeric(YY)<=01) {
    #   ruta <- paste0("data_raw/vegetacion/",YEAR,"NOAAVHMEDMNDVI/InfGeografica/InfRaster/TIF/NOAAVH_",YY,MM,"01_Andaluz_MEDMndvi.tif")
    # } else 
    if (as.numeric(YY)<=06) {
      ruta <- paste0("data_raw/vegetacion/",YEAR,"TERMODMEDMNDVI/InfGeografica/InfRaster/TIFF/TERMOD_",YY,MM,"01_h17v05_medmndvi.tif")
    } else if (as.numeric(YY)<=11) {
      ruta <- paste0("data_raw/vegetacion/",YEAR,"TERMODMEDMNDVI/InfGeografica/InfRaster/TIF/TERMOD_",YY,MM,"01_h17v05_medmndvi.tif")
    } else if (as.numeric(YY)<=21) {
      ruta <- paste0("data_raw/vegetacion/",YEAR,"TERMODMEDMNDVI/InfGeografica/InfRaster/TIFF/termod_",YY,MM,"01_h17v05_medmndvi.tif")
    }else {
      ruta <- paste0("data_raw/vegetacion/",YEAR,"TERMODMEDMNDVI/InfGeografica/InfRaster/COG/termod_",YY,MM,"01_h17v05_medmndvi_COG.tif")
    }
    
    if (file.exists(ruta)) {
      cat(YEAR,MONTH,"\n")
      # Observaciones en ese mes y año
      isMY = dataset$YEAR==YEAR & dataset$MM==MONTH
      if (any(isMY)) {
        NDVI_rast = as.numeric(rast(ruta))
        if (MONTH==4 & YEAR==2011){
          # Ese archivo viene defectuoso y le asigno el CRS de otros archivos del mismo año (todos los demás del año tienen el mismo)
          crs(NDVI_rast) = "PROJCRS[\"WGS 84 / UTM zone 30N\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"UTM zone 30N\",\n        METHOD[\"Transverse Mercator\",\n            ID[\"EPSG\",9807]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",-3,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"Scale factor at natural origin\",0.9996,\n            SCALEUNIT[\"unity\",1],\n            ID[\"EPSG\",8805]],\n        PARAMETER[\"False easting\",500000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Navigation and medium accuracy spatial referencing.\"],\n        AREA[\"Between 6°W and 0°W, northern hemisphere between equator and 84°N, onshore and offshore. Algeria. Burkina Faso. Côte' Ivoire (Ivory Coast). Faroe Islands - offshore. France. Ghana. Gibraltar. Ireland - offshore Irish Sea. Mali. Mauritania. Morocco. Spain. United Kingdom (UK).\"],\n        BBOX[0,-6,84,0]],\n    ID[\"EPSG\",32630]]"
        } 

        dataset[isMY,]$NDVI = terra::extract(NDVI_rast,dataset[isMY,])[,2]
      }
    } else
      cat("No existe: ",YEAR,"-",MONTH,"\n")
  } 
}


save(dataset,file = paste0("salidas_intermedias/dataset_completo",Sys.Date(),".RData"))

# Los archivos de enero a septiembre 2001 están defectuosos
# Los valores obtenidos en los años 2000 y 2001 son mucho más bajos que los de los otros años
# ante la falta de información al respecto decido omitirlos


# ndvi_mensual = dataset |> 
#   group_by(YEAR,MM) |> 
#   summarise(NDVI_mensual = median(NDVI,na.rm=T)) |> 
#   st_drop_geometry() |> 
#   unite("ym",YEAR:MM) |> 
#   as.data.frame()
# 
# 
# ndvi_mensual |> 
#   ggplot(mapping = aes(x = ym,y = NDVI_mensual)) +
#   geom_col()



# No existe 2000 - 2001 
# No existe 2003-01
# No existe 2003-04 
# No existe 2017-02 
# No existe 2018-11 
# No existe 2020-11 
# No existe 2021-12 
# No existe 2022-03 


# Falta Enero 2003
# Falta 22-03


# Apéndice ----------------------------------------------------------------


### ENP -----

# # Alternativa a rasterizar (muy lento)

# dataset$enp= st_within(dataset,enp,sparse=F)
# 
# rm(enp1,enp2,enp) 


### NDVI -----

# # Funciones que podrían ser útiles

# read_NDVI_vectorized <- function(MM, YYYY) {
#   MM <- str_pad(MM, 2, "left", pad = "0")  # Asegúrate de que MM tenga dos dígitos
#   YY <- substr(as.character(YYYY), 3, 4)   # Extrae los últimos dos dígitos del año
#   
#   ruta <- ifelse(as.numeric(YY) <= 6,
#                  paste0("data_raw/vegetacion/", YYYY, "TERMODMEDMNDVI/InfGeografica/InfRaster/TIFF/TERMOD_", YY, MM, "01_h17v05_medmndvi.tif"),
#                  ifelse(as.numeric(YY) <= 11,
#                         paste0("data_raw/vegetacion/", YYYY, "TERMODMEDMNDVI/InfGeografica/InfRaster/TIF/TERMOD_", YY, MM, "01_h17v05_medmndvi.tif"),
#                         ifelse(as.numeric(YY) <= 21,
#                                paste0("data_raw/vegetacion/", YYYY, "TERMODMEDMNDVI/InfGeografica/InfRaster/TIFF/termod_", YY, MM, "01_h17v05_medmndvi.tif"),
#                                paste0("data_raw/vegetacion/", YYYY, "TERMODMEDMNDVI/InfGeografica/InfRaster/COG/termod_", YY, MM, "01_h17v05_medmndvi_COG.tif")
#                         )
#                  )
#   )
#   
#   
#   # Leer los archivos raster correspondientes a las rutas existentes
#   NDVI <- ifelse(file.exists(ruta),
#                  purrr::map(ruta, rast),
#                  NA)
#   
#   # Devolver la lista de raster
#   return(NDVI)
# }


# datos = dataset[1:200,]
# 
# datos3 = datos %>%
#   select(-NDVI) |> 
#   group_by(YEAR, MM) %>%  # Agrupar por mes y año
#   nest() |> 
#   mutate(NDVI = map2(MM,YEAR,read_NDVI),
#          NDVI_point = map2(NDVI,data,~terra::extract(.x,.y) |> select(-ID) |> rename(NDVI_def=1))) |> 
#   select(-NDVI) |> 
#   unnest(c(data,NDVI_point))


# datos3 = datos |> 
#   select(-NDVI) |> 
#   group_by(YEAR, MM) |>  # Agrupar por mes y año
#   nest() |> 
#   mutate(NDVI_rast = map2(MM,YEAR,as.numeric(read_NDVI)),
#          NDVI = map2(NDVI_rast,data,~ifelse(!is.na(.x),terra::extract(.x,.y) |> select(-ID) |> rename(NDVI_def=1),NA))) |> 
#   #select(-NDVI) |> 
#   unnest(c(data,NDVI))

 
# datos_con_ndvi <- datos %>%
#   group_by(YEAR, MM) %>%  # Agrupar por mes y año
#   summarise(NDVI = list(read_NDVI_vectorized(MM, YEAR))) %>%  # Aplicar read_NDVI_vectorized una vez por mes y año
#   ungroup() %>%
#   mutate(extract_result = purrr::map2(NDVI, geometry, ~terra::extract(.x, .y))) 
#  


# read_NDVI = function(MM,YYYY) {
#   MM = str_pad(as.character(MM),2,"left",pad = "0")
#   YY = substr(as.character(YYYY),3,4)
#   if (as.numeric(YY)<=06) {
#     ruta <- paste0("data_raw/vegetacion/",YYYY,"TERMODMEDMNDVI/InfGeografica/InfRaster/TIFF/TERMOD_",YY,MM,"01_h17v05_medmndvi.tif")
#   } else if (as.numeric(YY)<=11) {
#     ruta <- paste0("data_raw/vegetacion/",YYYY,"TERMODMEDMNDVI/InfGeografica/InfRaster/TIF/TERMOD_",YY,MM,"01_h17v05_medmndvi.tif")
#   } else if (as.numeric(YY)<=21){
#     ruta <- paste0("data_raw/vegetacion/",YYYY,"TERMODMEDMNDVI/InfGeografica/InfRaster/TIFF/termod_",YY,MM,"01_h17v05_medmndvi.tif")
#   }else {
#     ruta <- paste0("data_raw/vegetacion/",YYYY,"TERMODMEDMNDVI/InfGeografica/InfRaster/COG/termod_",YY,MM,"01_h17v05_medmndvi_COG.tif")
#   }
#   
#   if (file.exists(ruta)) {
#     NDVI = rast(ruta)
#   } else
#     NDVI = NA
#   return(NDVI)
# }


# Prueba usando la api de MODIStools
# Problema: solo 1 localización, es inviable

# library(MODISTools)
# products <- mt_products()
# 
# arcachon_lai <- mt_subset(product = "MOD13Q1",
#                           lat = 36,
#                           lon =  -5,
#                           band = "250m_16_days_NDVI",
#                           start = "2003-01-01",
#                           end = "2003-01-31",
#                           km_lr = 20,
#                           km_ab = 20,
#                           site_name = "arcachon",
#                           internal = TRUE,
#                           progress = FALSE)


### Uso Suelo: ----

# load("data_cleaning/uso_suelo_reclasificado.RData")
# 
# st_crs(UsoSuelo_recl) == st_crs(dataset) # TRUE
# 
# # # Problema: es demasiado lento
# a = st_intersects(dataset,UsoSuelo_recl)
# 
# aa = sapply(a,function(x) ifelse(length(x)==0,"Otro",x))
# 
# aa = as.factor(aa)
# summary(aa)
# 
# b = dataset[aa=="Otro",1] |> st_drop_geometry()
# 
# # Vamos a simplificar la geometría para que que el archivo tenga menos vértices e intentar así
# # mejorar el rendimiento:
# as.numeric(object.size(UsoSuelo_recl)) # 243729376
# UsoSuelo_recl = UsoSuelo_recl |>
#   st_simplify(dTolerance = 100)
# as.numeric(object.size(UsoSuelo_recl)) #12912464


