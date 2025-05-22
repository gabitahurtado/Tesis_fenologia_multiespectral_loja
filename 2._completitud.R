############ ANALISIS DE PIXELES COMPELTOS EN EL INTERVALO DE ESTUDIO 


### LIBRERIAS

library(raster)
library(parallel)
library(ggplot2)
library(rgdal)
library(sf)
library(dplyr)
library(paletteer)
library(ggnewscale)
## vamos a ejecutar en paralelo la funcion para que sea mas rapido, dividiendo entre todos los nucleos el trabajo
modis = stack("D:/StarMedia/Users/GABY/Desktop/dats_fnls/COD_FN_TSS/archivos_origen/NDVI_17S_aqua_terra.tif")


# Inicializar un raster para almacenar la completitud
completitud <- raster(modis, layer=1)

# Función para calcular la completitud por pixel
calcCompletitud <- function(x) {
  completo <- sum(!is.na(x))  # Contar valores no NA
  porcentaje <- (completo / nlayers(modis)) * 100
  return(porcentaje)
}

# Aplicar la función a cada pixel
completitud <- calc(modis, calcCompletitud)

plot(completitud)



cultivos_pisos= shapefile("D:/StarMedia/Users/GABY/Desktop/dats_fnls/COD_FN_TSS/archivos_origen/CULTIVOS_PIS_CLM.shp")


completitud_corte = mask(completitud, cultivos_pisos)
plot(completitud_corte)


hist(completitud_corte)


completitud_corte ## raster de completitud 

cultivos_pisos #### aqui queiro graficar el USO AGROP


shp_pisos = shapefile("D:/StarMedia/Users/GABY/Desktop/dats_fnls/COD_FN_TSS/archivos_origen/PISOS.shp")
shp_loja = shapefile("D:/StarMedia/Users/GABY/Desktop/dats_fnls/COD_FN_TSS/archivos_origen/LOJA.shp")

#### creacion de mapa de completitud 
# Convertir a objetos sf
shp_pisos_sf <- st_as_sf(shp_pisos)
shp_loja_sf <- st_as_sf(shp_loja)
cultivos_pisos_sf = st_as_sf(cultivos_pisos)

# Transformar el raster a un formato que ggplot2 pueda usar
completitud_df <- as.data.frame(rasterToPoints(completitud_corte))
colnames(completitud_df) <- c("Longitud", "Latitud", "valor")

# Crear el mapa base
etiquetas_pisos = c("Tierras bajas", "Piemontano", "Montano bajo","Montano", "Montano alto")
# Construcción del gráfico

colores_pisos <- paletteer_d("RColorBrewer::PuBuGn")# Código hexadecimal para marrón y azul cielo
colores_completitud = c("#FFFF33", "#E61A33")

mapa_porcentaje = mapa_porcentaje + xlab("Longitud") +ylab("Latitud")

# Guardar el gráfico como un archivo TIFF
ggsave("mapa_porcentaje.tiff", mapa_porcentaje, width = 168, height = 168, units = "mm", dpi = 800)


##### cargar malla de puntos para la grafica 

malla_pnt = shapefile("D:/StarMedia/Users/GABY/Desktop/dats_fnls/COD_FN_TSS/archivos_origen/pnt_muestreo.shp")

# Extraer valores del raster en las ubicaciones de los puntos
ext_pornt<- extract(completitud_corte, malla_pnt)


# Convertir puntos a data.frame
malla_df <- as.data.frame(st_drop_geometry(malla_pnt))


# Unir los valores del raster con los puntos
puntos_con_valores <- cbind(malla_df, valor_porcnt=round(ext_pornt,0))
puntos_con_valores$USO_AGROP = as.factor(puntos_con_valores$USO_AGROP)


# Suponiendo que el dataframe se llama puntos_con_valores
puntos_con_valores <- puntos_con_valores %>%
  mutate(categorias_porcnt = case_when(
    valor_porcnt >= 60 & valor_porcnt < 70 ~ "60-70%",
    valor_porcnt >= 70 & valor_porcnt < 80 ~ "70-80%",
    valor_porcnt >= 80 & valor_porcnt < 90 ~ "80-90%",
    valor_porcnt >= 90 & valor_porcnt <= 100 ~ "90-100%",
    TRUE ~ as.character(valor_porcnt)  # por si hay valores fuera de los rangos definidos
  ))


puntos_con_valores2 <- puntos_con_valores %>%
  group_by(layer, USO_AGROP) %>%
  summarise(
    total = n(),
    count_60_70 = sum(categorias_porcnt == "60-70%"),
    count_70_80 = sum(categorias_porcnt == "70-80%"),
    count_80_90= sum(categorias_porcnt == "80-90%"),
    count_90_100 = sum(categorias_porcnt == "90-100%")
  ) %>%
  mutate(
    percentage_60_70 = (count_60_70 / total) * 100,
    percentage_70_80= (count_70_80 / total) * 100,
    percentage_80_90 = (count_80_90 / total) * 100,
    percentage_90_100 = (count_90_100 / total) * 100
    
    
  ) %>%
  pivot_longer(
    cols = starts_with("percentage"),
    names_to = "categorias_porcnt",
    values_to = "percentage",
    names_prefix = "percentage_"
  )%>%
  mutate(categorias_porcnt = case_when(
    categorias_porcnt== "60_70" ~ "60-70%",
    categorias_porcnt== "70_80" ~ "70-80%",
    categorias_porcnt== "80_90" ~ "80-90%",
    categorias_porcnt== "90_100" ~ "90-100%",
    TRUE ~ as.character(categorias_porcnt)  # por si hay valores fuera de los rangos definidos
  ))




# Reemplazar valores numéricos por etiquetas descriptivas en la columna USO_AGROP
puntos_con_valores2 <- puntos_con_valores2 %>%
  mutate(USO_AGROP = case_when(
    USO_AGROP == 1 ~ "Cultivo anual",
    USO_AGROP == 2 ~ "Cultivo permanente",
    USO_AGROP == 3 ~ "Cultivo semipermanente",
    USO_AGROP == 4 ~ "Mosaico agropecuario",
    USO_AGROP == 5 ~ "Pastizal",
    TRUE ~ as.character(USO_AGROP) # Mantener otros valores como están
  ))


# Filtrar para excluir la categoría "Mayores_3000"
puntos_con_valores2 <- puntos_con_valores2 %>%
  filter(layer != "Mayores_3000")%>%
  mutate(layer = factor(layer, levels = c("Montano_alto", "Montano", "Montano_bajo", "Piemontano", "Tierras_bajas")))%>%
  filter(layer != "Mayores_3000")


puntos_con_valores2 = puntos_con_valores2  %>%
  filter(!is.na(layer))  %>%
  filter(USO_AGROP!= "Cultivo permanente")


colores_slope_pv = c("black"  , "yellow", "#E18727" ,"#DC0000")
# Crear el gráfico excluyendo los NA en 'layer'
plot_porcnt = ggplot(puntos_con_valores2, aes(x = factor(USO_AGROP), y = percentage, fill = factor(categorias_porcnt))) + 
  geom_bar(stat = "identity", position = "fill", width = 0.5) + 
  scale_fill_manual(
    values = c("60-70%" = "#DC0000", "70-80%" = "yellow", "80-90%" = "#E18727", "90-100%"= "#DC0000") 
  ) +
  facet_grid( layer ~ ., labeller = labeller(layer = function(x) gsub("_", " ", x))) +
  labs(x = "Uso Agropecuario", y = "Porcentaje (%)", fill = "Píxeles") +
  theme_minimal() +
  theme(
    text = element_text(size = 0.8 * 11),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 7.5),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

plot_porcnt

# Guardar el gráfico como un archivo TIFF
ggsave("grafiica_porcentaje.tiff", plot_porcnt, width = 168, height = 168, units = "mm", dpi = 800)










###### exportar raster usado

writeRaster(completitud_corte, "raster_completitud.tif")
