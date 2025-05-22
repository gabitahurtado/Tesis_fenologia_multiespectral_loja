# Cargar las bibliotecas necesarias, si no lo has hecho aún
# install.packages("raster")  # Si no tienes instalada la librería "raster"
library(raster)

# Cargar el stack de imágenes MODIS NDVI
modis_stack_trr  <- stack("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/MODIS/NDVI_Monthly_2000_to_2019_Filtered.tif")
modis_stack_trr = modis_stack_trr *0.0001
# Cargar el stack de imágenes MODIS NDVI
modis_stack_aq <- stack("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/MODIS/NDVI_Monthly_2000_to_2019_Filtered_aqua.tif")
modis_stack_aq= modis_stack_aq *0.0001
# Definir los límites para reemplazar valores
lower_limit <- -1
upper_limit <- 1


# Reemplazar valores mayores a 1 por 1
modis_stack_trr[modis_stack_trr > upper_limit] <- 1

# Reemplazar valores menores a -1 por -1
modis_stack_trr[modis_stack_trr < lower_limit] <- -1




# Reemplazar valores mayores a 1 por 1
modis_stack_aq[modis_stack_aq > upper_limit] <- 1

# Reemplazar valores menores a -1 por -1
modis_stack_aq[modis_stack_aq < lower_limit] <- -1





############################## promedio entre terrra y agua para cada mes 
 ndvi_promedio= stack()

 ####### elimina las 29 primeras badnas de terra debido a que aqua no posee de estos meses informacion, solo tiene  a partir del 07-2002
 modis_stack_trr2 = modis_stack_trr[[1:29]] ### selecciona las 9 primeras bandas 
 
 modis_stack_trr3 = dropLayer(modis_stack_trr, c(1:29)) ### selecciona las 9 primeras bandas elimina las 29 priemras bandas
 



 
 ##### empieza el ciclo for para calcular el NDVI para cada mes entre aqua y terra 
 
 # Itera a través de cada capa en modis_stack_aq
 for (i in 1:nlayers(modis_stack_aq)) {
   # Calcula el promedio mensual entre las capas correspondientes de modis_stack_trr3 y modis_stack_aq
   promedio_mensual = mean(modis_stack_trr3[[i]], modis_stack_aq[[i]], na.rm = TRUE)
   
   # Establece los nombres de la capa resultante como los nombres de la capa de modis_stack_trr3
   names(promedio_mensual) = names(modis_stack_trr3[[i]])
   
   # Apila la capa de promedio_mensual en ndvi_promedio
   ndvi_promedio = stack(ndvi_promedio, promedio_mensual)
 }
 
 ndvi_promedio ###### este es el resultado de los promedios entre los meses que hay datos entre terra y aqua 
 
ndvi_final = stack(modis_stack_trr2,ndvi_promedio ) ### union del resto de bandas en las cuales solo hay informacion en terra



# Definir la proyección objetivo (UTM 17S)
utm_proj <- "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs"



# Reproyectar el stack a UTM 17S
ndvi_final <- projectRaster(ndvi_final , crs = utm_proj)


# Definir los límites para reemplazar valores
lower_limit <- -1
upper_limit <- 1

# Reemplazar valores mayores a 1 por 1
ndvi_final [ndvi_final  > upper_limit] <- 1

# Reemplazar valores menores a -1 por -1
ndvi_final [ndvi_final  < lower_limit] <- -1


# Exportar el resultado re-proyectado a un archivo
writeRaster(ndvi_final , filename = "C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/MODIS/NDVI_17S_aqua_terra.tif", format = "GTiff", overwrite=TRUE)


plot(ndvi_final$X49_NDVI)




