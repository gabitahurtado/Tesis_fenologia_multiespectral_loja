# Inicializa rasters para guardar los resultados
library(rstatix)
library(ggspatial) 
library(raster)

library(ggplot2)
library(rgdal)
library(sf)
library(dplyr)
library(paletteer)
library(ggnewscale)
library(tidyr)
#######LIBRERIAS
library(dplyr)
library(broom)## est
library(openxlsx)
modis_corte = brick("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/MODIS/MODIS_CORTE.tif")


modis_sum = sum(modis_corte, na.rm = T)

values_sum = values(modis_sum)

index_values = which( values_sum>0) #### estas son las ubicaciones donde existen valores 





temp_raster = brick("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/CHELSA/tem_med_corte17.tif")
temp_raster2 = temp_raster[[-1]]

cor_raster <- temp_raster2[[1]]
pval_raster <- temp_raster2[[1]]


values(cor_raster) <- NA
values(pval_raster) <- NA


NDVI_CLIMA2 <- read.csv("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/CODIGO/R/MODIS_DATA/NDVI_CLIMA2.csv", stringsAsFactors=TRUE)





### agregamos id con el cual distinguimos cada fila para despues saber que oulier elimiar 
NDVI_CLIMA2$id_fila = seq(1:length(NDVI_CLIMA2$X))
# Asumiendo que 'month' es un número de 1 a 12
# Crear un vector con las iniciales de los meses
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

# Convertir 'month' a factor con las iniciales de los meses como niveles
NDVI_CLIMA2$month2 <- factor(NDVI_CLIMA2$month, levels = 1:12, labels = meses)
# Crear una nueva columna para la clasificación
NDVI_CLIMA2$Epoca <- ifelse(NDVI_CLIMA2$month2 %in% c("Ene", "Feb", "Mar", "Abr"), "Lluviosa", "Seca")

outliers_temp = NDVI_CLIMA2 %>%
  filter(!is.na(valor_NDVI))%>%
  group_by(USO_AGROP,layer, year, Epoca) %>%
  identify_outliers(valor_TEMP )%>%
  filter(is.extreme == "TRUE")

dim(outliers_temp)
hist(outliers_temp$valor_TEMP)
# Aquí asumo que NDVI_CLIMA2 tiene una columna de índice única llamada 'id'.
### data frame sin ouliers de tempearura 

outliers_prec = NDVI_CLIMA2 %>%
  filter(!is.na(valor_NDVI))%>%
  group_by(USO_AGROP,layer, year, Epoca) %>%
  identify_outliers(valor_PRECI )%>%
  filter(is.extreme == "TRUE")

dim(outliers_prec)
hist(outliers_prec$valor_PRECI)


##### une los dos dataframes con los valroes atipicos 

ouliers_total = rbind(outliers_prec, outliers_temp )

ouliers_total = ouliers_total%>%distinct(id_fila, .keep_all = T)### elimina valores de id_fila repetidos 

dim(ouliers_total)
####### elimina los valores extremos encada piso altitudinal, cada cultivo, cada año en cada epoca 

NDVI_CLIMA2_clean <- NDVI_CLIMA2 %>%
  filter(!is.na(valor_NDVI))%>%
  
  anti_join(ouliers_total, by = "id_fila")

NDVI_CLIMA2_clean = na.omit(NDVI_CLIMA2_clean)
# Transformar el dataframe NDVI_temp_cor. Esta transformación se hace por grupos,
# definidos por la columna 'id'. Para cada grupo, se calcula la correlación de Spearman
# entre las columnas 'valor_TEMP' y 'valor_NDVI'
NDVI_temp_cor = NDVI_CLIMA2_clean  %>% 
  dplyr::select( id , USO_AGROP, layer, centro_x,centro_y,valor_TEMP,valor_NDVI )  %>% 
  group_by(id, centro_x,centro_y)   %>%  # Agrupar los datos por la columna 'id'
  summarise(
    # Calcular la correlación de Spearman para cada grupo
    Correlacion = {
      # Verificar primero si hay al menos dos observaciones no-NA
      if (sum(!is.na(valor_TEMP) & !is.na(valor_NDVI)) >= 2) {
        # Si hay suficientes observaciones, calcular la correlación de Spearman
        cor.test(valor_TEMP, valor_NDVI, method = "spearman", use = "complete.obs")$estimate
      } else {
        # Si no hay suficientes observaciones, asignar NA
        NA
      }
    },
    # Calcular el p-valor asociado con la correlación de Spearman para cada grupo
    pvalue = {
      # Nuevamente, verificar primero si hay al menos dos observaciones no-NA
      if (sum(!is.na(valor_TEMP) & !is.na(valor_NDVI)) >= 2) {
        # Si hay suficientes observaciones, calcular el p-valor
        cor.test(valor_TEMP, valor_NDVI, method = "spearman", use = "complete.obs")$p.value
      } else {
        # Si no hay suficientes observaciones, asignar NA
        NA
      }
    }
  )%>%
  mutate(
    # Crear una nueva columna llamada 'clasificacion'
    clasificacion = case_when(
      # Condición 1: Si 'Correlacion' es menor que 0 Y 'pvalue' es mayor que 0.05, asignar 1
      Correlacion <  0 & pvalue >= 0.05 ~ 1,
      
      # Condición 2: Si 'Correlacion' es mayor que 0 Y 'pvalue' es mayor que 0.05, asignar 2
      Correlacion  >0 & pvalue >= 0.05 ~ 2,
      
      # Condición 3: Si 'Correlacion' es menor que 0 Y 'pvalue' es menor  a 0.05, asignar 3
      Correlacion < 0 & pvalue < 0.05 ~ 3,
      
      # Condición 4: Si 'Correlacion' es mayor que 0 Y 'pvalue' es menor  a 0.05, asignar 4
      Correlacion > 0 & pvalue < 0.05 ~ 4,
      
      # Para cualquier otra condición no cubierta por las anteriores, asignar NA
      TRUE ~ NA_real_
    )
  )



### trasnforma el dataframe a un spatial points


coordinates(NDVI_temp_cor) <- ~centro_x+centro_y   # Establece las columnas de coordenadas
proj4string(NDVI_temp_cor) <- CRS("+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs")
NDVI_temp_cor


# Crear un raster vacío
raster_template <- raster(res = c(250, 250), 
                          xmn =  558032.9  , xmx = 707681.9 , 
                          ymn = 9469961, ymx = 9635249 )
crs(raster_template) = crs("+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs")
# Rasterizar los puntos


raster_cor_temp_class <- rasterize(NDVI_temp_cor, raster_template, field = "clasificacion",  background=NA, fun = "max")
# Reclassify -Inf to NA
# Función para reemplazar -Inf con NA
replace_inf_with_na <- function(x) {
  x[is.infinite(x)] <- NA
  return(x)
}

# Aplicar la función al raster
raster_cor_temp_class <- calc(raster_cor_temp_class, replace_inf_with_na)


plot(raster_cor_temp_class)
plot(shp_loja, add=T)



raster_cor_temp <- rasterize(NDVI_temp_cor, raster_template, field = "Correlacion", background=NA, fun = max)

# Aplicar la función al raster
raster_cor_temp <- calc(raster_cor_temp, replace_inf_with_na)



plot(raster_cor_temp)
plot(shp_loja, add=T)











raster_cor_temp_df <- as.data.frame(rasterToPoints(raster_cor_temp_class))
colnames(raster_cor_temp_df) <- c("longitud", "latitud", "valor")


colores_pisos <- paletteer_d("RColorBrewer::PuBuGn")# Código hexadecimal para marrón y azul cielo
# Etiquetas personalizadas para la leyenda
etiquetas_cor_temp <- c("Correlación - y p.valor >0.05",
                        "Correlación + y p.valor >0.05",
                        "Correlación - y p.valor <0.05",
                        "Correlación + y p.valor <0.05")

etiquetas_pisos = c("Tierras bajas", "Piemontano", "Montano bajo","Montano", "Montano alto", "Mayor a 3000 m.s.n.m.")

colores_temp = c("black"  , "yellow", "#E18727" ,"#DC0000")
# Construcción del gráfico

# Construcción del gráfico
mapa_temp_cor <- ggplot() +
  # Añade la capa de pisos climáticos con sus respectivos colores
  geom_sf(data = shp_pisos_sf, aes(fill = as.factor(pisclim_si)), color = NA, size = 0.001) +
  # Asigna los colores definidos a los pisos climáticos y nombra la leyenda como "Pisos Climáticos"
  scale_fill_manual(values = colores_pisos, name = "Pisos Climáticos",labels = etiquetas_pisos) +
  # Inicia una nueva escala de colores para la siguiente capa
  new_scale_fill() +
  # Añade la capa de pendientes con su escala de color
  geom_tile(data = raster_cor_temp_df %>% filter(!is.na(valor)), aes(x = longitud, y = latitud, fill = factor(valor))) +
  # Asigna los colores y etiquetas definidos a las pendientes y nombra la leyenda como "Categorías"
  scale_fill_manual(values = colores_temp, labels =etiquetas_cor_temp, name = "Categorías") +
  # Añade la capa de contornos de la provincia de Loja con líneas negras y sin relleno
  geom_sf(data = shp_loja_sf, fill = NA, color = "black", size = 0.001, alpha= 0.8) +
  # Añade una flecha de norte en la esquina superior izquierda del mapa
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in")) +
  # Añade una barra de escala en la esquina inferior izquierda del mapa
  annotation_scale(location = "bl", width_hint = 0.3) +
  # Configura el tema del gráfico para un diseño minimalista y ajusta el tamaño y tipo de letra de todos los textos
  theme_minimal() +
  theme(
    text = element_text(size = 8, family = "Courier"),  # Establece el tamaño y la familia de fuentes para todo el texto
    legend.position = "right",  # Posiciona las leyendas en el lado derecho del gráfico
    legend.direction = "vertical",  # Las leyendas se muestran verticalmente
    legend.key.size = unit(1, "lines"),  # Ajusta el tamaño de los elementos de la leyenda
    legend.title = element_text(size = 8),  # Tamaño del texto del título de la leyenda
    legend.text = element_text(size = 8),  # Tamaño del texto de los elementos de la leyenda
    plot.title = element_text(size = 10, face = "bold.italic", hjust = 0.5),  # Título del gráfico con estilo en negrita cursiva
    panel.background = element_rect(fill = "white", colour = NA),  # Fondo blanco para el panel
    plot.background = element_rect(fill = "white", colour = NA)  # Fondo blanco para todo el gráfico
  )  +
  # Etiquetas para el título y leyendas de los ejes
  labs(
    title = "Mapa de correlación NDVI - Temperatura",
    fill = "Completitud",
    color = "Uso Agrícola",
    x= "Longitud",
    y = "Latitud"
  )

print(mapa_temp_cor)

# Guardar el gráfico como un archivo TIFF
ggsave("mapa_cor_temp.tiff", mapa_temp_cor, width = 168, height = 168, units = "mm", dpi = 800)






writeRaster(raster_cor_temp,"correlacio_temp.tif", overwrite=TRUE) #### valores de 1 son estadisticamene significativos 

writeRaster(raster_cor_temp_class,"clsificacion_correlacion_raster_temp.tif", overwrite=TRUE) #### valores de 1 son estadisticamene significativos 


######## plot de temepratura por piso altitudinal 







##### cargar malla de puntos para la grafica 

malla_pnt = shapefile("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/PUNTOS_MUESTREO/pnt_muestreo.shp")

# Extraer valores del raster en las ubicaciones de los puntos
ext_class<- raster::extract(raster_cor_temp_class, malla_pnt)


# Convertir puntos a data.frame
malla_df <- as.data.frame(st_drop_geometry(malla_pnt))


# Unir los valores del raster con los puntos
puntos_con_valores <- cbind(malla_df, valor_class=as.factor(ext_class))
puntos_con_valores$USO_AGROP = as.factor(puntos_con_valores$USO_AGROP)



puntos_con_valores2 <- puntos_con_valores %>%
  mutate(
    valor_pvalue = case_when(
      valor_class == 1 ~ 0,
      valor_class == 2 ~ 0,
      valor_class == 3 ~ 1,
      valor_class == 4 ~ 1,
      TRUE ~ NA_real_  # Para otros casos, puedes asignar NA o algún valor por defecto
    ),
    valor_corr = case_when(
      valor_class == 1 ~ 0,
      valor_class == 2 ~ 1,
      valor_class == 3 ~ 0,
      valor_class == 4 ~ 1,
      TRUE ~ NA_real_  # Similarmente aquí para otros casos
    )
  )%>% filter( valor_pvalue == 1)%>%
  filter( valor_pvalue == 1) %>%
  group_by(layer, USO_AGROP) %>%
  summarise(
    total = n(),
    count_1 = sum(valor_corr == 1),
    count_0 = sum(valor_corr == 0)
  ) %>%
  mutate(
    percentage_1 = (count_1 / total) * 100,
    percentage_0 = (count_0 / total) * 100
  ) %>%
  pivot_longer(
    cols = starts_with("percentage"),
    names_to = "valor_cor",
    values_to = "percentage",
    names_prefix = "percentage_"
  )



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
  filter(USO_AGROP!= "Cultivo permanente")%>%
  filter(!is.na(valor_cor))  



# Ver el resultado
print(puntos_con_valores2)







write.xlsx(puntos_con_valores2, file = "porcentaje_relacion_temp.xlsx")




# Crear el gráfico excluyendo los NA en 'layer'
plot_por_piso_temp =  ggplot(puntos_con_valores2, aes(fill = factor(valor_cor), y = percentage, x = as.factor(USO_AGROP))) + 
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  scale_fill_manual(
    values = c("0" = "#E18727", "1" = "#DC0000"),
    labels = c("0" = "Negativa - p.valor < 0.05", "1" = "Positiva - p.valor < 0.05")
  ) +
  facet_grid(layer~ ., labeller = labeller(layer = function(x) gsub("_", " ", x))) +
  labs(x = "Uso Agropecuario", y = "Porcentaje (%)", fill = "Correlación NDVI-Temperatura (°C)") +
  theme_minimal() +
  theme(
    text = element_text(size = 0.8 * 11),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 7.5),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

plot_por_piso_temp


# Guardar el gráfico como un archivo TIFF
ggsave("correlacion_piso_temperatura.tiff", plot_por_piso_temp, width = 168, height = 168, units = "mm", dpi = 800)




##########################
###############  grafica de ejemplo de un pixel con corrrelacion positiva estaidsticamente significativa 
##########################


id_pixel_positivo_temp=  puntos_con_valores %>%
  filter(valor_class == 4)%>%##### la lista de pixeles con los valores que podriamos usar para la grafica 
  pull(id)



pixel_positivo_plot = NDVI_CLIMA2_clean%>%
  filter( id %in% id_pixel_positivo_temp )%>%
  group_by(layer, USO_AGROP) %>% # Agrupar por Layer y USO_AGROP
  sample_n(size = 3, replace = TRUE) %>% # Tomar 3 muestras aleatorias por grupo
  ungroup() 

relacion_positiva_pixel = ggplot(pixel_positivo_plot, aes(x = valor_TEMP, y = valor_NDVI)) +
  geom_point() +
  theme_minimal() +
  facet_grid(layer ~ USO_AGROP, 
             labeller = labeller(layer = function(x) gsub("_", " ", x))) +
  geom_smooth(method="loess", se=TRUE, fullrange=FALSE, level=0.95)+
  labs(
    
    x = "Temperatura promedio mensual °C",
    y = "NDVI"
    
  )+
  theme_minimal() +
  theme(
    text = element_text(size = 0.8 * 11),  # Ajustar el tamaño del texto
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5), # Rotar y hacer más pequeñas las etiquetas del eje X
    panel.background = element_rect(fill = "white", colour = NA),  # Fondo blanco para el panel
    plot.background = element_rect(fill = "white", colour = NA)  # Fondo blanco para todo el gráfico
    
  ) 


relacion_positiva_pixel



ggsave("relacio_pixel_temperatura_ndvi_positiva.tiff", relacion_positiva_pixel, width = 168, height = 168, units = "mm", dpi = 800)







##########################
###############  grafica de ejemplo de un pixel con corrrelacion negativa  estaidsticamente significativa 
##########################


id_pixel_negativo_temp=  puntos_con_valores %>%
  filter(valor_class == 3)%>%##### la lista de pixeles con los valores que podriamos usar para la grafica 
  pull(id)

id_aleatorio <- sample(id_pixel_negativo_temp, 3)

pixel_negativo_plot = NDVI_CLIMA2_clean%>%
  filter( id ==  id_aleatorio  )

relacion_negativo_pixel = ggplot(pixel_negativo_plot, aes(x = valor_TEMP, y = valor_NDVI, col = as.factor(id))) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method="loess", se=TRUE, fullrange=FALSE, level=0.95)+
  labs(
    x = "Temperatura promedio mensual °C",
    y = "NDVI",
    col = "ID - Píxel"
  )+
  theme_minimal() +
  theme(
    text = element_text(size = 0.8 * 11),  # Ajustar el tamaño del texto
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5), # Rotar y hacer más pequeñas las etiquetas del eje X
    panel.background = element_rect(fill = "white", colour = NA),  # Fondo blanco para el panel
    plot.background = element_rect(fill = "white", colour = NA)  # Fondo blanco para todo el gráfico
    
  ) 


relacion_negativo_pixel



ggsave("relacio_pixel_temperatura_ndvi_negativa.tiff", relacion_negativo_pixel, width = 168, height = 168, units = "mm", dpi = 800)
















###### realiza lo mismo para rpecipitacion 

# Transformar el dataframe NDVI_temp_cor. Esta transformación se hace por grupos,
# definidos por la columna 'id'. Para cada grupo, se calcula la correlación de Spearman
# entre las columnas 'valor_TEMP' y 'valor_NDVI'
NDVI_preci_cor = NDVI_CLIMA2_clean  %>% 
  dplyr::select( id , USO_AGROP, layer, centro_x,centro_y,valor_PRECI ,valor_NDVI )  %>% 
  group_by(id, centro_x,centro_y)   %>%  # Agrupar los datos por la columna 'id'
  summarise(
    # Calcular la correlación de Spearman para cada grupo
    Correlacion = {
      # Verificar primero si hay al menos dos observaciones no-NA
      if (sum(!is.na(valor_PRECI ) & !is.na(valor_NDVI)) >= 2) {
        # Si hay suficientes observaciones, calcular la correlación de Spearman
        cor.test(valor_PRECI , valor_NDVI, method = "spearman", use = "complete.obs")$estimate
      } else {
        # Si no hay suficientes observaciones, asignar NA
        NA
      }
    },
    # Calcular el p-valor asociado con la correlación de Spearman para cada grupo
    pvalue = {
      # Nuevamente, verificar primero si hay al menos dos observaciones no-NA
      if (sum(!is.na(valor_PRECI) & !is.na(valor_NDVI)) >= 2) {
        # Si hay suficientes observaciones, calcular el p-valor
        cor.test(valor_PRECI, valor_NDVI, method = "spearman", use = "complete.obs")$p.value
      } else {
        # Si no hay suficientes observaciones, asignar NA
        NA
      }
    }
  )%>%
  mutate(
    # Crear una nueva columna llamada 'clasificacion'
    clasificacion = case_when(
      # Condición 1: Si 'Correlacion' es mayor que 0 Y 'pvalue' es mayor que 0.05, asignar 1
      Correlacion > 0 & pvalue >= 0.05 ~ 1,
      
      # Condición 2: Si 'Correlacion' es menor que 0 Y 'pvalue' es mayor que 0.05, asignar 2
      Correlacion < 0 & pvalue >= 0.05 ~ 2,
      
      # Condición 3: Si 'Correlacion' es menor que 0 Y 'pvalue' es menor  a 0.05, asignar 3
      Correlacion < 0 & pvalue < 0.05 ~ 3,
      
      # Condición 4: Si 'Correlacion' es mayor que 0 Y 'pvalue' es menor  a 0.05, asignar 4
      Correlacion > 0 & pvalue < 0.05 ~ 4,
      
      # Para cualquier otra condición no cubierta por las anteriores, asignar NA
      TRUE ~ NA_real_
    )
  )



### trasnforma el dataframe a un spatial points


coordinates(NDVI_preci_cor) <- ~centro_x+centro_y   # Establece las columnas de coordenadas
proj4string(NDVI_preci_cor) <- CRS("+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs")
NDVI_preci_cor


# Crear un raster vacío
raster_template <- raster(res = c(250, 250), 
                          xmn =  558032.9  , xmx = 707681.9 , 
                          ymn = 9469961, ymx = 9635249 )
crs(raster_template) = crs("+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs")
# Rasterizar los puntos
raster_cor_preci_class <- rasterize(NDVI_preci_cor, raster_template, field = "clasificacion", fun = "max")

raster_cor_preci_class <- calc(raster_cor_preci_class, replace_inf_with_na)


plot(raster_cor_preci_class)
plot(shp_loja, add=T)



raster_cor_prec<- rasterize(NDVI_preci_cor, raster_template, field = "Correlacion", fun = "max")
raster_cor_prec <- calc(raster_cor_prec, replace_inf_with_na)


plot(raster_cor_prec)
plot(shp_loja, add=T)









raster_cor_preci_df <- as.data.frame(rasterToPoints(raster_cor_preci_class))
colnames(raster_cor_preci_df) <- c("Longitud", "Latitud", "valor")


colores_pisos <- paletteer_d("RColorBrewer::PuBuGn")# Código hexadecimal para marrón y azul cielo
# Etiquetas personalizadas para la leyenda
etiquetas_cor_temp <- c("Correlación - y p.valor >0.05",
                        "Correlación + y p.valor >0.05",
                        "Correlación - y p.valor <0.05",
                        "Correlación + y p.valor <0.05")

etiquetas_pisos = c("Tierras bajas", "Piemontano", "Montano bajo","Montano", "Montano alto", "Mayor a 3000 m.s.n.m.")

colores_temp = c("black"  , "yellow", "#E18727" ,"#DC0000")
# Construcción del gráfico

# Construcción del gráfico
mapa_preci_cor <- ggplot() +
  # Añade la capa de pisos climáticos con sus respectivos colores
  geom_sf(data = shp_pisos_sf, aes(fill = as.factor(pisclim_si)), color = NA, size = 0.001) +
  # Asigna los colores definidos a los pisos climáticos y nombra la leyenda como "Pisos Climáticos"
  scale_fill_manual(values = colores_pisos, name = "Pisos Climáticos",labels = etiquetas_pisos) +
  # Inicia una nueva escala de colores para la siguiente capa
  new_scale_fill() +
  # Añade la capa de pendientes con su escala de color
  geom_tile(data = raster_cor_preci_df %>% filter(!is.na(valor)), aes(x = longitud, y = latitud, fill = factor(valor))) +
  # Asigna los colores y etiquetas definidos a las pendientes y nombra la leyenda como "Categorías"
  scale_fill_manual(values = colores_temp, labels =etiquetas_cor_temp, name = "Categorías") +
  # Añade la capa de contornos de la provincia de Loja con líneas negras y sin relleno
  geom_sf(data = shp_loja_sf, fill = NA, color = "black", size = 0.001, alpha= 0.8) +
  # Añade una flecha de norte en la esquina superior izquierda del mapa
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in")) +
  # Añade una barra de escala en la esquina inferior izquierda del mapa
  annotation_scale(location = "bl", width_hint = 0.3) +
  # Configura el tema del gráfico para un diseño minimalista y ajusta el tamaño y tipo de letra de todos los textos
  theme_minimal() +
  theme(
    text = element_text(size = 8, family = "Courier"),  # Establece el tamaño y la familia de fuentes para todo el texto
    legend.position = "right",  # Posiciona las leyendas en el lado derecho del gráfico
    legend.direction = "vertical",  # Las leyendas se muestran verticalmente
    legend.key.size = unit(1, "lines"),  # Ajusta el tamaño de los elementos de la leyenda
    legend.title = element_text(size = 8),  # Tamaño del texto del título de la leyenda
    legend.text = element_text(size = 8),  # Tamaño del texto de los elementos de la leyenda
    plot.title = element_text(size = 10, face = "bold.italic", hjust = 0.5),  # Título del gráfico con estilo en negrita cursiva
    panel.background = element_rect(fill = "white", colour = NA),  # Fondo blanco para el panel
    plot.background = element_rect(fill = "white", colour = NA)  # Fondo blanco para todo el gráfico
  )  +
  # Etiquetas para el título y leyendas de los ejes
  labs(
    title = "Mapa de correlación NDVI - Precipitación",
    fill = "Completitud",
    color = "Uso Agrícola",
    x= "Longitud",
    y = "Latitud"
  )

print(mapa_preci_cor)

# Guardar el gráfico como un archivo TIFF
ggsave("mapa_cor_preci.tiff", mapa_preci_cor, width = 168, height = 168, units = "mm", dpi = 800)















##### cargar malla de puntos para la grafica 

malla_pnt = shapefile("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/PUNTOS_MUESTREO/pnt_muestreo.shp")

# Extraer valores del raster en las ubicaciones de los puntos
ext_class<- raster::extract(raster_cor_preci_class, malla_pnt)


# Convertir puntos a data.frame
malla_df <- as.data.frame(st_drop_geometry(malla_pnt))


# Unir los valores del raster con los puntos
puntos_con_valores <- cbind(malla_df, valor_class=as.factor(ext_class))
puntos_con_valores$USO_AGROP = as.factor(puntos_con_valores$USO_AGROP)



# Calculando el porcentaje de cada valor_pvalue dentro de cada valor_slp
puntos_con_valores2 <- puntos_con_valores %>%
  mutate(
    valor_pvalue = case_when(
      valor_class == 1 ~ 0,
      valor_class == 2 ~ 0,
      valor_class == 3 ~ 1,
      valor_class == 4 ~ 1,
      TRUE ~ NA_real_  # Para otros casos, puedes asignar NA o algún valor por defecto
    ),
    valor_corr = case_when(
      valor_class == 1 ~ 0,
      valor_class == 2 ~ 1,
      valor_class == 3 ~ 0,
      valor_class == 4 ~ 1,
      TRUE ~ NA_real_  # Similarmente aquí para otros casos
    )
  )%>% filter( valor_pvalue == 1)%>%
  filter( valor_pvalue == 1) %>%
  group_by(layer, USO_AGROP) %>%
  summarise(
    total = n(),
    count_1 = sum(valor_corr == 1),
    count_0 = sum(valor_corr == 0)
  ) %>%
  mutate(
    percentage_1 = (count_1 / total) * 100,
    percentage_0 = (count_0 / total) * 100
  ) %>%
  pivot_longer(
    cols = starts_with("percentage"),
    names_to = "valor_cor",
    values_to = "percentage",
    names_prefix = "percentage_"
  )

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
  filter(USO_AGROP!= "Cultivo permanente")%>%
filter(!is.na(valor_cor))  


# Ver el resultado
print(puntos_con_valores2)


write.xlsx(puntos_con_valores2, file = "porcentaje_relacion_preci.xlsx")


# Crear el gráfico

# Crear el gráfico excluyendo los NA en 'layer'
plot_por_piso_prec= ggplot(puntos_con_valores2, aes(fill = factor(valor_cor), y = percentage, x = as.factor(USO_AGROP))) + 
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  scale_fill_manual(
    values = c("0" = "#E18727", "1" = "#DC0000"),
    labels = c("0" = "Negativa - p.valor < 0.05", "1" = "Positiva - p.valor < 0.05")
  ) +
  facet_grid(layer~ ., labeller = labeller(layer = function(x) gsub("_", " ", x))) +
  labs(x = "Uso Agropecuario", y = "Porcentaje (%)", fill = "Correlación NDVI- Precipitación mensual") +
  theme_minimal() +
  theme(
    text = element_text(size = 0.8 * 11),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 7.5),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )




plot_por_piso_prec
# Guardar el gráfico como un archivo TIFF
ggsave("plot_por_piso_prec.tiff", plot_por_piso_prec, width = 168, height = 168, units = "mm", dpi = 800)

##########################
###############  grafica de ejemplo de un pixel con corrrelacion positiva estaidsticamente significativa 
##########################



id_pixel_positivo_prec=  puntos_con_valores %>%
  filter(valor_class == 4)%>%##### la lista de pixeles con los valores que podriamos usar para la grafica 
  pull(id)

id_aleatorio <- sample(id_pixel_positivo_prec, 3)

pixel_positivo_plot = NDVI_CLIMA2_clean%>%
  filter( id ==  id_aleatorio  )

relacion_positiva_pixel = ggplot(pixel_positivo_plot, aes(x = valor_PRECI, y = valor_NDVI, col = as.factor(id))) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method="loess", se=TRUE, fullrange=FALSE, level=0.95)+
  labs(
    
    x = "Precipitación (mm/mes)",
    y = "NDVI",
    fill = "ID - Pixel"
  )+
  theme_minimal() +
  theme(
    text = element_text(size = 0.8 * 11),  # Ajustar el tamaño del texto
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5), # Rotar y hacer más pequeñas las etiquetas del eje X
    panel.background = element_rect(fill = "white", colour = NA),  # Fondo blanco para el panel
    plot.background = element_rect(fill = "white", colour = NA)  # Fondo blanco para todo el gráfico
    
  ) 
relacion_positiva_pixel

# Guardar el gráfico como un archivo TIFF
ggsave("relacion_prec_ndvi_pixel.tiff", relacion_positiva_pixel, width = 168, height = 168, units = "mm", dpi = 800)


writeRaster(raster_cor_prec,"correlacio_preci.tif", overwrite=TRUE) #### valores de 1 son estadisticamene significativos 

writeRaster(raster_cor_preci_class,"clsificacion_correlacion_raster_preci.tif", overwrite=TRUE) #### valores de 1 son estadisticamene significativos 
