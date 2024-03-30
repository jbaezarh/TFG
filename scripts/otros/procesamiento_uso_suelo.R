library(terra)
library(sf)
library(mapSpain)
library(tidyverse)
library(climate)
library(lubridate)

# Uso de suelo ------------------------------------------------------------

UsoSuelo <- read_sf("data_raw/antropologicas/UsoSuelo/06_01_UsoSuelo.shp")

# Lo paso a tipo factor
UsoSuelo$uso_suelo = as.factor(UsoSuelo$uso_suelo)
UsoSuelo$cod_uso = as.integer(UsoSuelo$cod_uso)

str(UsoSuelo)

# Solo me interesanta discriminar las zonas de bosque de frondosas, de coníferas o mixto,
# las zonas de matorral, zonas húmedas y zonas de escasa vegetación

# Bosques
bosque_frondosas = filter(UsoSuelo,uso_suelo=="Bosques de frondosas") |> st_union() 
bosque_coniferas = filter(UsoSuelo,uso_suelo=="Bosques de coníferas") |> st_union()
bosque_mixto = filter(UsoSuelo,uso_suelo=="Bosque mixto") |> st_union()

# Zonas de matorral:
matorral = filter(UsoSuelo, between(cod_uso,321,324)) |> st_union()

# Zonas con poca vegetación:
vegetacion_escasa =  filter(UsoSuelo, between(cod_uso,331,335)) |> st_union()

# Zonas húmedas:
zonas_humedas = filter(UsoSuelo, between(cod_uso,411,423)) |> st_union()


# Vamos a crear un nuevo archivo vectorial con la reclasifición hecha del uso del suelo:

categorias = c("Bosques de frondosas","Bosques de coníferas", "Bosque mixto","Matorral",
               "Vegetación escasa","Zonas húmedas")

UsoSuelo_geom = c(bosque_frondosas,bosque_coniferas,bosque_mixto,
             matorral,vegetacion_escasa,zonas_humedas)

UsoSuelo_recl = st_sf(uso_suelo = categorias,geometry = UsoSuelo_geom)
UsoSuelo_recl # reclasificación usos de suelo

# Lo guardamos
# save(UsoSuelo_recl,file = "data_cleaning/uso_suelo_reclasificado.RData" )




?rasterize

# Lo usaremos como modelo:
pend <- rast("data_raw/topograficas/pendiente.tif")
str(UsoSuelo)
UsoSuelo_rast <- rasterize(UsoSuelo,pend,
                           field="cod_uso")

# save(UsoSuelo_rast,file = "data_cleaning/uso_suelo_rast.RData" ) # No funciona
# writeRaster(UsoSuelo_rast, filename = "data_cleaning/uso_suelo_rast.tiff")

us <- UsoSuelo_rast

load("salidas_intermedias/dataset_compl_us.RData")
load("data_cleaning/uso_suelo_rast.RData") #  No va

UsoSuelo_rast <- rast("data_cleaning/uso_suelo_rast.tiff")

uso_suelo = terra::extract(UsoSuelo_rast,sample[dataset$fire==1,])[,2]

table(uso_suelo$cod_uso)

# Tal vez la clave sería combinar las categorías 1, 4 y 5 dentro de cada una



montes <- read_sf("D:/usuario/Documents/Universidad/5º/TFG bien/Código/raw_data/094-dera-11-patrimonio-6f7g/11_15_Montes.shp")
