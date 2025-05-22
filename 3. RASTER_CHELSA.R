library(raster)

# Directorio principal
directorio_principal <- "C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/CHELSA/PRECIPITACIÓN"

# Obtener la lista de carpetas dentro del directorio principal
carpetas <- list.dirs(directorio_principal, full.names = TRUE, recursive = FALSE)

# Crear un stack para almacenar los archivos raster
stack_raster <- stack()

# Iterar sobre cada carpeta
for (carpeta in carpetas) {
  archivos_raster <- list.files(path = carpeta, pattern = ".tif$", full.names = TRUE)
  
  archivos_ordenados <- archivos_raster[order(as.numeric(gsub(".*pp(\\d+)_\\d+\\.tif", "\\1", archivos_raster)))]
  
  # Leer cada archivo raster y añadirlo al stack
  for (archivo in archivos_ordenados) {
    raster_actual <- raster(archivo)
    stack_raster <- stack(stack_raster, raster_actual)
  }
}


precipitacion = stack_raster/100

# Guardar el stack como un archivo raster
writeRaster(precipitacion, filename = "C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/CHELSA/precipitacion_total.tif",overwrite=TRUE, format = "GTiff")



# Directorio principal de temperatura máxima
directorio_principal <- "C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/CHELSA/TEMPERATURA/TEMP_MAX"

# Obtener la lista de carpetas dentro del directorio principal
carpetas <- list.dirs(directorio_principal, full.names = TRUE, recursive = FALSE)

# Crear un stack para almacenar los archivos raster
stack_raster <- stack()

# Iterar sobre cada carpeta
for (carpeta in carpetas) {
  archivos_raster <- list.files(path = carpeta, pattern = ".tif$", full.names = TRUE)
  
  archivos_ordenados <- archivos_raster[order(as.numeric(gsub(".*tx(\\d+)_\\d+\\.tif", "\\1", archivos_raster)))]
  
  # Leer cada archivo raster y añadirlo al stack
  for (archivo in archivos_ordenados) {
    raster_actual <- raster(archivo)
    stack_raster <- stack(stack_raster, raster_actual)
  }
}

temperatura_maxima <- stack_raster










# Directorio principal de temperatura promedio 
directorio_principal <- "C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/CHELSA/TEMPERATURA/TEMP_MED"

# Obtener la lista de carpetas dentro del directorio principal
carpetas <- list.dirs(directorio_principal, full.names = TRUE, recursive = FALSE)

# Crear un stack para almacenar los archivos raster
stack_raster <- stack()

# Iterar sobre cada carpeta
for (carpeta in carpetas) {
  archivos_raster <- list.files(path = carpeta, pattern = ".tif$", full.names = TRUE)
  archivos_ordenados <- archivos_raster[order(as.numeric(gsub(".*tm(\\d+)_\\d+\\.tif", "\\1", archivos_raster)))]
  
  # Leer cada archivo raster y añadirlo al stack
  for (archivo in archivos_ordenados) {
    raster_actual <- raster(archivo)
    stack_raster <- stack(stack_raster, raster_actual)
  }
}

temperatura_med <- stack_raster/100# Directorio principal de temperatura promedio 






# Guardar el stack como un archivo raster
writeRaster(temperatura_med , filename = "C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/CHELSA/tem_med_total.tif",overwrite=TRUE, format = "GTiff")





# Directorio principal de temperatura promedio 
directorio_principal <- "C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/CHELSA/TEMPERATURA/TEMP_MIN"

# Obtener la lista de carpetas dentro del directorio principal
carpetas <- list.dirs(directorio_principal, full.names = TRUE, recursive = FALSE)

# Crear un stack para almacenar los archivos raster
stack_raster <- stack()

# Iterar sobre cada carpeta
for (carpeta in carpetas) {
  archivos_raster <- list.files(path = carpeta, pattern = ".tif$", full.names = TRUE)
  archivos_ordenados <- archivos_raster[order(as.numeric(gsub(".*tn(\\d+)_\\d+\\.tif", "\\1", archivos_raster)))]
  
  # Leer cada archivo raster y añadirlo al stack
  for (archivo in archivos_ordenados) {
    raster_actual <- raster(archivo)
    stack_raster <- stack(stack_raster, raster_actual)
  }
}

temperatura_min<- stack_raster# Directorio principal de temperatura promedio 

