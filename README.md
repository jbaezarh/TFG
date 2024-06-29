# Modelos de predicción de incendios forestales
En el presente trabajo fin de estudios se aborda el problema de la predicción diaria de incendios forestales en la Comunidad Autónoma de Andalucía haciendo uso de técnicas de procesamiento de datos espaciales y modelos de *Machine Learning*.  Se fija el marco temporal del estudio entre los años 2002 y 2022. Se consideran 27 variables correspondientes a 6 categorías: antropogénica, meteorológica, topográfica, demográfica, hidrológica y de vegetación. Se usan los perímetros de incendios forestales mayores de 100 *ha* ocurridos en Andalucía y obtenidos a partir de imágenes satélite y datos de campo disponibles en la Red de Información Ambiental de Andalucía (REDIAM). Se implementan métodos para procesar los conjuntos de datos espaciales recopilados y generar muestras adecuadas para entrenar modelos predictivos, con los cuales se genera una muestra de 21.546 registros que se usa para entrenar los modelos, considerando una partición temporal en entrenamiento-validación-test. Los modelos analizados han sido: Regresión Logística con penalización, Regresión Logística con penalización usando PCA, k-*Nearest Neighbours*, SVM lineal, SVM radial, Árboles de Decisión y *Random Forest*. Se han ajustado los valores de los hiperparámetros evaluando el rendimiento sobre el conjunto de validación y se ha comparado el rendimiento de los modelos construidos sobre el conjunto test considerando diversas métricas. Han destacado los modelos de Regresión Logística *lasso* y SVM, que han obtenido los mejores resultados en el conjunto test. Finalmente, se ha evaluado el desempeño de estos modelos en dos casos prácticos, obteniendo resultados prometedores.

En este repositorio se puede encontrar todo el código usado en el trabajo fin de estudios, así como todos los conjuntos de datos recopilados y generados. Se incluyen, además, la memoria (`TFE_DGME_BaezaRuizHenestrosa_Juan.pdf`) y las diapositivas usadas para su defensa (`TFE_DGME_BaezaRuizHenestrosa_Juan.pdf`).

Con el objetivo facilitar la colaboración en el proyecto y hacer que el contenido de este sea completamente reproducible, se ha usado el paquete de *R* *renv*, que permite crear entornos de trabajo reproducibles para los proyectos de *R*. Gracias al uso de este paquete, el proyecto tiene su propia librería local, y cualquiera que lo abra dispondrá de todos los paquetes utilizados en la misma versión ha sido empleada en el proyecto con solo correr `renv::restore()`.

Los scripts con el código se encuentran en el directorio `scripts/`. 

Los conjuntos de datos procesados se encuentran en el directorio `salidas_intermedias/`. En el código se indica el archivo adecuado en cada caso.

NOTA: La carpeta con los datos brutos (`data_raw`) contiene archivos demasiado pesados para subirlos a GitHub, pero puede descargarse desde [este enlace](https://drive.google.com/file/d/1_gczTaw8lQybE6GgeZo6E4jgF9MvhzpS/view?usp=drive_link) , basta descomprimirla y copiar la carpeta `data_raw` en el directorio principal.

 
