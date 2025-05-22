# Cargar las librerías necesarias
library(sp)
library(raster)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)  # Librería adicional para manipulación de datos

# Leer el archivo de Excel con los datos
pnt_muestreo_ndvi <- read_excel("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/PUNTOS_MUESTREO/pnt_aquea_terra_ndvi.xlsx")

# Mostrar la columna USO_AGROP
pnt_muestreo_ndvi$USO_AGROP

# Definir las etiquetas para reemplazar valores numéricos en USO_AGROP
reemplazos <- c("Cultivo anual", "Cultivo permanente", "Cultivo semipermanente", "Mosaico agropecuario", "Pastizal")

# Reemplazar valores numéricos por etiquetas descriptivas en la columna USO_AGROP
pnt_muestreo_ndvi <- pnt_muestreo_ndvi %>%
  mutate(USO_AGROP = case_when(
    USO_AGROP == 1 ~ "Cultivo anual",
    USO_AGROP == 2 ~ "Cultivo permanente",
    USO_AGROP == 3 ~ "Cultivo semipermanente",
    USO_AGROP == 4 ~ "Mosaico agropecuario",
    USO_AGROP == 5 ~ "Pastizal",
    TRUE ~ as.character(USO_AGROP) # Mantener otros valores como están
  ))

# Convertir la columna USO_AGROP a tipo factor
pnt_muestreo_ndvi$USO_AGROP = as.factor(pnt_muestreo_ndvi$USO_AGROP)

# Generar un vector de fechas mensuales desde el 2000-01-01 hasta el 2019-12-01
fechas <- seq(as.Date("2000-01-01"), as.Date("2019-12-01"), by = "1 month")
fechas_formato <- format(fechas, "%Y-%m")

# Eliminar la primera fecha (la fecha de inicio)
fechas_formato = fechas_formato[-1]

# Crear nombres de columnas con el formato "NDVI_aaaa-mm"
fechas_formato = paste("NDVI_", fechas_formato, sep = "")

# Seleccionar columnas necesarias para el análisis
pnt_muestreo_ndvi2 = pnt_muestreo_ndvi %>% 
  select(-FID_dissol, -pisclim_si, -FID_diss_1)

# Renombrar las columnas seleccionadas con los nombres de fechas generados
colnames(pnt_muestreo_ndvi2)[8:dim(pnt_muestreo_ndvi2)[2]] = fechas_formato

# Transformar el dataframe al formato largo
pnt_muestreo_ndvi3 <- pnt_muestreo_ndvi2 %>%
  pivot_longer(cols = starts_with("NDVI"),
               names_to = "fecha",
               values_to = "valor_NDVI") %>%
  separate(fecha, into = c("NDVI", "fecha"), sep = "_") %>%
  unite("fecha", c("fecha", "NDVI"), sep = "-")


# Eliminar el sufijo "-NDVI" de la columna "fecha" y convertirla a formato fecha
pnt_muestreo_ndvi4 <-pnt_muestreo_ndvi3 %>%
  mutate(fecha = sub("-NDVI", "", fecha))




# Separar la columna "fecha" en año y mes
pnt_muestreo_ndvi4 <- transform(pnt_muestreo_ndvi4, 
                       year = as.integer(substr(fecha, 1, 4)), # Extraer el año
                       month = as.integer(substr(fecha, 6, 7)) # Extraer el mes
)


# Crear una nueva columna de fecha a partir del año y mes extraídos
pnt_muestreo_ndvi4$fecha <- as.Date(paste(pnt_muestreo_ndvi4$year, pnt_muestreo_ndvi4$month, "01", sep = "-"))


# Calcula el centroide para cada fila
pnt_muestreo_ndvi4$centro_x <- (pnt_muestreo_ndvi4$right +pnt_muestreo_ndvi4$left) / 2
pnt_muestreo_ndvi4$centro_y <- (pnt_muestreo_ndvi4$top + pnt_muestreo_ndvi4$bottom) / 2

head(pnt_muestreo_ndvi4)



# Instalar el paquete si no lo tienes instalado
# install.packages("openxlsx")

library(openxlsx)


write.xlsx(pnt_muestreo_ndvi4, "C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/PUNTOS_MUESTREO/pnt_muestreo_ndvi_final.xlsx")


##### añadir los meses que son epoca luviosa y epoca secaa



# Mostrar el resultado
print(pnt_muestreo_ndvi4)


ggplot(pnt_muestreo_ndvi4, aes(x=layer, y = valor_NDVI, fill = USO_AGROP ))+
  geom_boxplot()



ggplot(pnt_muestreo_ndvi4, aes(x=USO_AGROP, y = valor_NDVI, fill = layer ))+
  geom_boxplot()


