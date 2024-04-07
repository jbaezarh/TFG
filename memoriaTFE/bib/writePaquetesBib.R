# Escribir bibliografía paquetes

library(knitr)

knitr::write_bib(c("terra"
                   ,"sf"
                   ,"mapSpain"
                   ,"tidyverse"
                   ,"nasapower"
                   ,"raster"),file = "bib/paquetes.bib",width = 60)

