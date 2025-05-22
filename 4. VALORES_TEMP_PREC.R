# Cargar las librerías necesarias
library(sp)
library(raster)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)  # Librería adicional para manipulación de datos

# Leer el archivo de Excel con los datos
pnt_muestreo_preci <- read_excel("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/PUNTOS_MUESTREO/pnt_preci.xlsx")


# Generar un vector de fechas mensuales desde el 2000-01-01 hasta el 2019-12-01
fechas <- seq(as.Date("2000-01-01"), as.Date("2019-06-01"), by = "1 month")
fechas_formato <- format(fechas, "%Y-%m")



# Crear nombres de columnas con el formato "NDVI_aaaa-mm"
fechas_formato = paste("PRECI_", fechas_formato, sep = "")

# Seleccionar columnas necesarias para el análisis
pnt_muestreo_preci2 =pnt_muestreo_preci %>% 
  select(-FID_dissol, -pisclim_si, -FID_diss_1)

# Renombrar las columnas seleccionadas con los nombres de fechas generados
colnames(pnt_muestreo_preci2)[8:dim(pnt_muestreo_preci2)[2]] = fechas_formato

# Transformar el dataframe al formato largo
pnt_muestreo_preci3 <- pnt_muestreo_preci2 %>%
  pivot_longer(cols = starts_with("PRECI"),
               names_to = "fecha",
               values_to = "valor_PRECI") %>%
  separate(fecha, into = c("PRECI", "fecha"), sep = "_") %>%
  unite("fecha", c("fecha", "PRECI"), sep = "-")


# Eliminar el sufijo "-NDVI" de la columna "fecha" y convertirla a formato fecha
pnt_muestreo_preci4 <-pnt_muestreo_preci3%>%
  mutate(fecha = sub("-PRECI", "", fecha))




# Separar la columna "fecha" en año y mes
pnt_muestreo_preci4 <- transform(pnt_muestreo_preci4, 
                                year = as.integer(substr(fecha, 1, 4)), # Extraer el año
                                month = as.integer(substr(fecha, 6, 7)) # Extraer el mes
)


# Crear una nueva columna de fecha a partir del año y mes extraídos
pnt_muestreo_preci4$fecha <- as.Date(paste(pnt_muestreo_preci4$year,pnt_muestreo_preci4$month, "01", sep = "-"))


pnt_muestreo_preci4 = pnt_muestreo_preci4%>%select(id, valor_PRECI, year, month)




#########################################################################################################################





########################### temperstura 

# Leer el archivo de Excel con los datos
pnt_muestreo_temp <- read_excel("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/PUNTOS_MUESTREO/pnt_temp.xlsx")


# Generar un vector de fechas mensuales desde el 2000-01-01 hasta el 2019-12-01
fechas <- seq(as.Date("2000-01-01"), as.Date("2019-12-01"), by = "1 month")
fechas_formato <- format(fechas, "%Y-%m")



# Crear nombres de columnas con el formato "NDVI_aaaa-mm"
fechas_formato = paste("TEMP_", fechas_formato, sep = "")

# Seleccionar columnas necesarias para el análisis
pnt_muestreo_temp2 =pnt_muestreo_temp %>% 
  select(-FID_dissol, -pisclim_si, -FID_diss_1)

# Renombrar las columnas seleccionadas con los nombres de fechas generados
colnames(pnt_muestreo_temp2)[8:dim(pnt_muestreo_temp2)[2]] = fechas_formato

# Transformar el dataframe al formato largo
pnt_muestreo_temp3 <- pnt_muestreo_temp2%>%
  pivot_longer(cols = starts_with("TEMP"),
               names_to = "fecha",
               values_to = "valor_TEMP") %>%
  separate(fecha, into = c("TEMP", "fecha"), sep = "_") %>%
  unite("fecha", c("fecha", "TEMP"), sep = "-")


# Eliminar el sufijo "-NDVI" de la columna "fecha" y convertirla a formato fecha
pnt_muestreo_temp4 <-pnt_muestreo_temp3%>%
  mutate(fecha = sub("-TEMP", "", fecha))




# Separar la columna "fecha" en año y mes
pnt_muestreo_temp4 <- transform(pnt_muestreo_temp4, 
                                 year = as.integer(substr(fecha, 1, 4)), # Extraer el año
                                 month = as.integer(substr(fecha, 6, 7)) # Extraer el mes
)


# Crear una nueva columna de fecha a partir del año y mes extraídos
pnt_muestreo_temp4$fecha <- as.Date(paste(pnt_muestreo_temp4$year,pnt_muestreo_temp4$month, "01", sep = "-"))


pnt_muestreo_temp4 = pnt_muestreo_temp4%>%select(id, valor_TEMP, year, month)

###############################################################

############# UNION CON BASES DE DATOS #########################

#############################################################


NDVI_PRECI = left_join(pnt_muestreo_ndvi4,pnt_muestreo_preci4, by =c("id", "year", "month") )




NDVI_CLIMA = left_join(NDVI_PRECI,pnt_muestreo_temp4, by =c("id", "year", "month") )

NDVI_CLIMA <- NDVI_CLIMA [NDVI_CLIMA$USO_AGROP != "Cultivo permanente", ]


NDVI_CLIMA <- NDVI_CLIMA [NDVI_CLIMA$layer != "Mayores_3300", ]


######################################### DEM 




########################### temperstura 

# Leer el archivo de Excel con los datos
pnt_muestreo_DEM <- read_excel("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/PUNTOS_MUESTREO/pnt_DEM.xlsx")


# Seleccionar columnas necesarias para el análisis
pnt_muestreo_DEM2 =pnt_muestreo_DEM %>% 
  dplyr::select(id, DEM_1)




NDVI_CLIMA2 = left_join(NDVI_CLIMA ,pnt_muestreo_DEM2, by =c("id") )

write.csv(NDVI_CLIMA2, "NDVI_CLIMA2.csv", row.names = FALSE)
