######################## ANALISIS DESCRIPTIVO DE LAS VARIABLES 


#######LIBRERIAS
library(dplyr)
library(broom)## esta libreria permite odenar los resulados de las pruebas estadisticas

### carga de datos extraidos con los centroides
NDVI_CLIMA2 <- read.csv("D:/StarMedia/Users/GABY/Desktop/dats_fnls/COD_FN_TSS/NDVI_CLIMA2.csv", stringsAsFactors=TRUE)


######## NDVI 

analisis_descriptivo_ndvi<- NDVI_CLIMA2 %>%
  group_by(layer, USO_AGROP) %>%
  summarise(
    Media_NDVI = mean(valor_NDVI, na.rm = TRUE),
    SD_NDVI = sd(valor_NDVI, na.rm = TRUE),
    .groups = 'drop'
  )


# Test de Shapiro-Wilk por layer y USO_AGROP
resultado_shapiro_ndvi <- NDVI_CLIMA2 %>%
  group_by(layer, USO_AGROP) %>%
  sample_n(size = min(5000, n()), replace = TRUE) %>%
  do(tidy(shapiro.test(.$valor_NDVI))) %>%
  ungroup() %>%
  select(layer, USO_AGROP, p.value)


# Unir los resultados del análisis descriptivo con los del test de Shapiro
resultado_final_ndvi <- left_join(analisis_descriptivo_ndvi, resultado_shapiro_ndvi, by = c("layer", "USO_AGROP"))

# Ver los resultados
print(resultado_final_ndvi)

##### temperatura 

analisis_descriptivo_temp<- NDVI_CLIMA2 %>%
  group_by(layer, USO_AGROP) %>%
  summarise(
    Media_temp = mean(valor_TEMP, na.rm = TRUE),
    SD_temp = sd(valor_TEMP, na.rm = TRUE),
    .groups = 'drop'
  )


# Test de Shapiro-Wilk por layer y USO_AGROP
resultado_shapiro_temp <- NDVI_CLIMA2 %>%
  group_by(layer, USO_AGROP) %>%
  sample_n(size = min(5000, n()), replace = TRUE) %>%
  do(tidy(shapiro.test(.$valor_TEMP))) %>%
  ungroup() %>%
  select(layer, USO_AGROP, p.value)


# Unir los resultados del análisis descriptivo con los del test de Shapiro
resultado_final_temp <- left_join(analisis_descriptivo_temp , resultado_shapiro_temp , by = c("layer", "USO_AGROP"))

# Ver los resultados
print(resultado_final_temp )






##### precipitacion 

analisis_descriptivo_preci<- NDVI_CLIMA2 %>%
  group_by(layer, USO_AGROP) %>%
  summarise(
    Media_preci = mean(valor_PRECI, na.rm = TRUE),
    SD_preci = sd(valor_PRECI, na.rm = TRUE),
    .groups = 'drop'
  )


# Test de Shapiro-Wilk por layer y USO_AGROP
resultado_shapiro_preci <- NDVI_CLIMA2 %>%
  group_by(layer, USO_AGROP) %>%
  sample_n(size = min(5000, n()), replace = TRUE) %>%
  do(tidy(shapiro.test(.$valor_PRECI))) %>%
  ungroup() %>%
  select(layer, USO_AGROP, p.value)


# Unir los resultados del análisis descriptivo con los del test de Shapiro
resultado_final_preci = left_join(analisis_descriptivo_preci , resultado_shapiro_preci , by = c("layer", "USO_AGROP"))

# Ver los resultados
print(resultado_final_preci )



#### exportar resultados 


library(openxlsx)
# Crear un nuevo libro de Excel
wb <- createWorkbook()

# Añadir una hoja con los resultados de Shapiro-Wilk
addWorksheet(wb, "analisis descriptivo NDVI")
writeData(wb, "analisis descriptivo NDVI", resultado_final_ndvi)

# Guardar el libro de Excel, asegúrate de especificar tu ruta deseada
saveWorkbook(wb, "D:/StarMedia/Users/GABY/Desktop/dats_fnls/COD_FN_TSS/Resultados_Analisis.xlsx", overwrite = TRUE)

# Para añadir nuevos resultados en una nueva hoja, carga el libro existente
wb <- loadWorkbook("D:/StarMedia/Users/GABY/Desktop/dats_fnls/COD_FN_TSS/Resultados_Analisis.xlsx")

# Añadir una nueva hoja para otros resultados
# Asegúrate de cambiar 'nuevos_resultados' por tu nuevo dataframe de resultados
addWorksheet(wb, "analisis descriptivo temp")
writeData(wb, "analisis descriptivo temp", resultado_final_temp)

# Guardar nuevamente el libro con los cambios
saveWorkbook(wb, "D:/StarMedia/Users/GABY/Desktop/dats_fnls/COD_FN_TSS/Resultados_Analisis.xlsx", overwrite = TRUE)




wb <- loadWorkbook("D:/StarMedia/Users/GABY/Desktop/dats_fnls/COD_FN_TSS/Resultados_Analisis.xlsx")

# Añadir una nueva hoja para otros resultados
# Asegúrate de cambiar 'nuevos_resultados' por tu nuevo dataframe de resultados
addWorksheet(wb, "analisis descriptivo preci")
writeData(wb, "analisis descriptivo preci", resultado_final_preci)

# Guardar nuevamente el libro con los cambios
saveWorkbook(wb, "D:/StarMedia/Users/GABY/Desktop/dats_fnls/COD_FN_TSS/Resultados_Analisis.xlsx", overwrite = TRUE)

