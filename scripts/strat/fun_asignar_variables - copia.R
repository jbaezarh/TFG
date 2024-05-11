library(nasapower)
library(raster, include.only = c("rasterFromXYZ"))  # Función para construir rasters a partir de data.frames
library(tidyverse)
library(sf)
library(terra)
library(mapSpain)

# Asigna todas las variables explicativas a un conjunto de localizaciones con fechas

# sample : sf object with columns geometry and date

asignar_variables = function(sample) {
  
  crs_reference = st_crs(sample)
  and = esp_get_ccaa(ccaa = "Andalucía") %>% st_transform(st_crs(datos)) 
  
  # Variables meteorológicas ------------------------------------------------
  cat("Asignando variables meteorológicas...\n")
  
  # Tranformamos los datos a WGS84
  andalucia_WGS84 <- st_transform(and,crs="WGS84")
  
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
  
  # save(dataset,file = paste0("salidas_intermedias/dataset_clim",Sys.Date(),".RData"))
  
  # Variables topográficas --------------------------------------------------
  cat("Asignando variables topográficas...\n")
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
  cat("Asignando variables antropológicas...\n")
  
##### Para optimizar el cálculo evitando que se repitan cálculos si hay puntos repetidos:
  dataset_geoms <- dataset %>% 
    group_by(geometry) %>% 
    group_keys() %>% 
    st_sf(crs = st_crs(dataset)) 
  
  ### Carreteras: ----
  carreteras <- read_sf("data_raw/antropologicas/RedCarreteras/09_14_RedCarreteras.shp") |> 
    st_union()
  
  dataset_geoms$dist_carretera <- st_distance(dataset_geoms,carreteras) |> 
    as.numeric()      # metres
  
  rm(carreteras)
  
  
  ### Poblaciones: ----
  poblaciones <- read_sf("data_raw/antropologicas/Poblaciones/07_01_Poblaciones.shp") |> 
    st_union()
  
  dataset_geoms$dist_poblacion <- st_distance(dataset_geoms,poblaciones) |> 
    as.numeric()    # metres
  
  rm(poblaciones)
  
  
  ### Linea Eléctrica: ----
  linea_electrica <- read_sf("data_raw/antropologicas/LineaElectrica/10_14_LineaElectrica.shp") |> 
    st_union()
  
  dataset_geoms$dist_electr <- st_distance(dataset_geoms,linea_electrica) |> 
    as.numeric() # metres
  
  rm(linea_electrica)
  
  ### Ferrocarril: ----
  ferrocarril <- read_sf("data_raw/antropologicas/Ferrocarril/09_21_Ferrocarril.shp") |> 
    st_union()
  dataset_geoms$dist_ferrocarril <- st_distance(dataset_geoms,ferrocarril) |> 
    as.numeric()
  
  rm(ferrocarril)
  
  ### Camino / Via: ----
  camino <- read_sf("data_raw/antropologicas/Camino/09_19_Camino.shp") 
  viapec <- read_sf("data_raw/antropologicas/Camino/09_22_ViasPecuarias.shp") 
  
  camino_viapec <- c(st_geometry(camino),st_geometry(viapec))
  rm(camino,viapec)
  
  camino_viapec <- st_union(camino_viapec)
  
  dataset_geoms$dist_camino <- st_distance(dataset_geoms,camino_viapec) |> 
    as.numeric()
  
  rm(camino_viapec)
  
  ### Sendero / Vía Verde / CarrilBici: ----
  viaverde <- read_sf("data_raw/antropologicas/Sendero_ViaVerde/09_24_ViaVerde.shp") 
  sendero <- read_sf("data_raw/antropologicas/sendero_ViaVerde/09_20_Sendero.shp") 
  carrilbic <- read_sf("data_raw/antropologicas/sendero_ViaVerde/09_23_CarrilBici.shp")
  
  sendero_viaverde_carrilbici <- c(st_geometry(viaverde),st_geometry(sendero),st_geometry(carrilbic)) |> 
    st_union()
  
  dataset_geoms$dist_sendero <- st_distance(dataset_geoms,sendero_viaverde_carrilbici) |> 
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
  dataset_geoms$enp= terra::extract(enp_rast,dataset_geoms)[,2]
  
  rm(enp,enp1,enp2,enp_sf,enp_rast)
  
  ### Uso Suelo: ----
  # UsoSuelo <- read_sf("data_raw/antropologicas/UsoSuelo/06_01_UsoSuelo.shp")
  # UsoSuelo_rast <- rasterize(UsoSuelo,
  #                            rast("data_raw/topograficas/pendiente.tif"), # Nos sirve de modelo
  #                            field="cod_uso")
  
  UsoSuelo_rast <- rast("data_cleaning/uso_suelo_rast.tiff")
  
  dataset_geoms$uso_suelo = terra::extract(UsoSuelo_rast,dataset_geoms)[,2]
  
  # save(dataset_geoms,file = paste0("salidas_intermedias/dataset_geoms_clim_antr",Sys.Date(),".RData"))
  
  
  # Hidrográficas -----------------------------------------------------------
  cat("Asignando variables hidrográficas...\n")
  # Distancia a ríos
  rios <- read_sf("data_raw/hidrograficas/Rios_Espana.shp") |> 
    st_transform(st_crs(dataset)) |> 
    st_crop(xmin = 100394.4, # Esto se hace solo para no tener que considerar todo el file y que sea más manipulable
            ymin = 3976888.6,
            xmax = 690000.8,
            ymax = 4350000.0) |> 
    st_union()
  
  dataset_geoms$dist_rios <- st_distance(dataset_geoms,rios) |> 
    as.numeric() # metres
  
  rm(rios)
  
####  Y ahora se asocian los valores a las geometrías correspondientes
  dataset <- dataset %>% 
    st_join(dataset_geoms,left = TRUE) # Es un left join espacial
  
  
  # Demográficas -----------------------------------------------------------
  cat("Asignando variables demográficas...\n")
  ### Población y densidad de población: ----
  
  # Asigno población y densidad de población:
  
  poblacion <- read_csv2("data_raw/antropologicas/Población/poblacion_municipios.txt",
                         locale=locale(decimal_mark = ","),
                         col_select = 1:5,col_types = "ccifn") |> 
    mutate(Valor=as.integer(round(Valor))) # Por algún motivo aparecen decimales
  
  # https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/operaciones/consulta/anual/28867?CodOper=b3_151&codConsulta=28867
  area_municipios <- read_csv2("data_raw/antropologicas/Población/extension_municipal.txt",
                               locale=locale(decimal_mark = ","),
                               col_select = 1:6, col_types = "fffffn")
  # Andalucia: 87592.742053km2
  
  area_municipios <- area_municipios %>% 
    filter(!is.na(CODIGO_INE3)) %>% 
    select(CODIGO_INE3,Valor) %>% 
    rename("Area" = "Valor")
  
  
  # Se calcula la densidad de población
  dens_poblacion <- poblacion %>% 
    select(-Medida) %>% 
    rename("Poblacion" = "Valor",
           "Municipio" = "Lugar de residencia") %>% 
    left_join(area_municipios,
              join_by("CODIGO_INE3")) %>% 
    mutate(dens_poblacion = Poblacion/Area) %>% 
    select(-Area)
  
  municipios <- esp_get_munic(epsg = 4258,region = "Andalucía")
  # plot(st_geometry(municipios))
  
  municipios <- municipios |> 
    st_transform(crs_reference)
  
  # Asociamos a cada observacion su codigo de municipio correspondiente 
  
  num_mun = st_intersects(dataset,municipios) 
  
  # Eliminamos las observaciones que no están en ningún municipio:
  if (any(sapply(num_mun,function(x) length(x) == 0))) {
    cat("Eliminamos las observaciones:\n",which(sapply(num_mun,function(x) length(x) == 0)))
    dataset = dataset[-which(sapply(num_mun,function(x) length(x) == 0)),]
  }
  
  dataset$cod_municipio <- municipios[unlist(st_intersects(dataset,municipios)),]$LAU_CODE
  
  dataset <- dataset |> 
    left_join(dens_poblacion,
              join_by(cod_municipio==CODIGO_INE3,YEAR==Anual)) |> 
    rename("municipio" = "Municipio",
           "poblacion" = "Poblacion")
  
  
  # Vegetación --------------------------------------------------------------
  cat("Asignando variables de vegetación...\n")
  dataset$NDVI = NA
  
  for (YEAR in 2002:2022) {
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
  
  # "Depuración":
  # --------------
  
  dataset <- dataset |> 
    mutate(enp = as.factor(enp),
           orientacion = cut(orientacion,
                             breaks = c(-Inf,-1,22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5,360),
                             labels = c("Plano","N","NE","E","SE","S","SW","W","NW","N")),
           WD10M = cut(WD10M,
                       breaks = c(0,22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5,360),
                       labels = c("N","NE","E","SE","S","SW","W","NW","N")),
           uso_suelo = uso_suelo |> as.character() |> str_sub(0,2) |> as.factor()
           ) %>% 
    select(-c(YEAR,MM,DD))
  
  return(dataset)
}