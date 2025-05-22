#### tendencias temporales pixel por pixel 
### carga recorte de modis

library(ggspatial) 
library(raster)

library(ggplot2)
library(rgdal)
library(sf)
library(dplyr)
library(paletteer)
library(ggnewscale)
library(tidyr)
library(Kendall)
library(zyp)
######################## serie de tiempo unsado pruebas Tessi Juan Maita 
# Leer el archivo raster
modis_corte <- brick("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/MODIS/MODIS_CORTE.tif")
time <- 1:nlayers(modis_corte)

# Función para el test de Mann-Kendall Contextual
mkc_fun = function(x) {
  if (all(is.na(x))) {
    return(c(NA, NA))  # Devuelve NA si todos los valores son NA
  } else {
    mk_test <- MannKendall(( x))
    return(c(mk_test$tau, mk_test$sl))  # Devuelve tau y p-value
  }
}


# Calcular Mann-Kendall Contextual para cada píxel
mkc_result <- calc(modis_corte, mkc_fun)


plot(mkc_result, main=" Test de MK") ### la pirmera badna en la correlacion y la segunda banda es el p valor 

ts_fun <- function(x) {
  # Asegurarse de que hay suficientes datos no-NA para realizar la regresión
  if (sum(!is.na(x)) < 2) {
    return(NA_real_)
  }
  
  
  # Crear un data frame temporal para la regresión
  time <- 1:length( x )
  data_temp <- data.frame(time = time, value = x)
  
  # Realizar la regresión Theil-Sen
  ts_result <- zyp.sen(value ~ time, data = na.omit(data_temp))
  
  # Devolver solo la pendiente
  return( mean(ts_result$slopes))
}

# Ahora puedes aplicar ts_fun al raster completo
ts_slope <- calc(modis_corte, ts_fun)


# Multiplicar la pendiente por el factor de conversión si es necesario
ts_slope_cnv <- ts_slope * 239

# Visualización
plot(ts_slope_cnv, main="Pendiente de Theil-Sen (Convertida)")






############ grafica de mapa y de barras por piso suando emtodologia de Maita 



##### union de resultados 
## msumamos el resultado de la pendiente +2
modis.cor_mk= mkc_result[[1]] > 0 ##### hacemos una clasificacion de valores psoitivos y negativos 
plot(modis.cor_mk)

modis.cor_mk_2= modis.cor_mk +2 ### se hace una covnersion para luego clasificar , se suma el dos

plot(modis.cor_mk_2)
#### valores de 3 son relaciones positivas y valores de 2 son relaciones negativas 


## msumamos el resultado de la pendiente +2

modis.pvalue_mk= mkc_result[[2]]  < 0.05 ###hacemos uan clasificacion para ver cuales son significativps
###### valores de 1 son significativos y valores de 0 no lo son 
plot(modis.pvalue_mk)
modis.pvalue_mk2= modis.pvalue_mk*2
plot(modis.pvalue_mk2)
#### valores de 2 son estadisticamente significativos valores de 0 no lo son 


##### sumamos  por la clasificacion del p valor

cor_pval =modis.cor_mk_2 + modis.pvalue_mk2
plot(cor_pval )
#### valores de 2 son pendientes engativas no signficiativas 
### valores de 3 son pendientes positivas nos signficiativas
#### vaores de 4 son pendientes negativas significatrivas 
##### y valores de 5 son pendientes psoitivas significativas 


#####

#### grafica de mapa 
colores_pisos <- paletteer_d("RColorBrewer::PuBuGn")# Código hexadecimal para marrón y azul cielo
slope_pv_df <- as.data.frame(rasterToPoints(cor_pval))
colnames(slope_pv_df) <- c("longitud", "latitud", "valor")

# Etiquetas personalizadas para la leyenda
etiquetas_slope_pv <- c("Pendiente - y p.valor >0.05",
                        "Pendiente + y p.valor >0.05",
                        "Pendiente - y p.valor <0.05",
                        "Pendiente + y p.valor <0.05")

etiquetas_pisos = c("Tierras bajas", "Piemontano", "Montano bajo","Montano", "Montano alto", "Mayor a 3000 m.s.n.m.")

colores_slope_pv = c("black"  , "yellow", "#E18727" ,"#DC0000")
# Construcción del gráfico

# Construcción del gráfico
mapa_mk <- ggplot() +
  # Añade la capa de pisos climáticos con sus respectivos colores
  geom_sf(data = shp_pisos_sf, aes(fill = as.factor(pisclim_si)), color = NA, size = 0.001) +
  # Asigna los colores definidos a los pisos climáticos y nombra la leyenda como "Pisos Climáticos"
  scale_fill_manual(values = colores_pisos, name = "Pisos Climáticos",labels = etiquetas_pisos) +
  # Inicia una nueva escala de colores para la siguiente capa
  new_scale_fill() +
  # Añade la capa de pendientes con su escala de color
  geom_tile(data = slope_pv_df %>% filter(!is.na(valor)), aes(x = longitud, y = latitud, fill = factor(valor))) +
  # Asigna los colores y etiquetas definidos a las pendientes y nombra la leyenda como "Categorías"
  scale_fill_manual(values = colores_slope_pv, labels = etiquetas_slope_pv, name = "Significancia de cambio") +
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
    title = "Mapa de significancia del cambio del NDVI",
    fill = "Completitud",
    x= "Longitud",
    y = "Latitud"
  )

print(mapa_mk)


# Guardar el gráfico como un archivo TIFF
ggsave("Mapa correlaciones MK.tiff", mapa_mk, width = 168, height = 168, units = "mm", dpi = 800)


##### cargar malla de puntos para la grafica 

malla_pnt = shapefile("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/DATOS/PUNTOS_MUESTREO/pnt_muestreo.shp")

# Extraer valores del raster en las ubicaciones de los puntos
ext_slp<- raster::extract(modis.cor_mk, malla_pnt)
ext_pvlue = raster::extract(modis.pvalue_mk, malla_pnt)

# Convertir puntos a data.frame
malla_df <- as.data.frame(st_drop_geometry(malla_pnt))


# Unir los valores del raster con los puntos
puntos_con_valores <- cbind(malla_df, valor_slp=as.factor(ext_slp), valor_pvalue = as.factor(ext_pvlue))
puntos_con_valores$USO_AGROP = as.factor(puntos_con_valores$USO_AGROP)



puntos_con_valores2 <- puntos_con_valores %>%
  filter( valor_pvalue == 1) %>%
  group_by(layer, USO_AGROP) %>%
  summarise(
    total = n(),
    count_1 = sum(valor_slp == 1),
    count_0 = sum(valor_slp == 0)
  ) %>%
  mutate(
    percentage_1 = (count_1 / total) * 100,
    percentage_0 = (count_0 / total) * 100
  ) %>%
  pivot_longer(
    cols = starts_with("percentage"),
    names_to = "valor_slp",
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
  filter(USO_AGROP!= "Cultivo permanente")
  
  

# Crear el gráfico

# Crear el gráfico excluyendo los NA en 'layer'
plot_por_piso_mk =  ggplot(puntos_con_valores2, aes(fill = factor(valor_slp), y = percentage, x = as.factor(USO_AGROP))) + 
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  scale_fill_manual(
    values = c("0" = "#E18727", "1" = "#DC0000"),
    labels = c("0" = "Negativa - p.valor < 0.05", "1" = "Positiva - p.valor < 0.05")
  ) +
  facet_grid(layer~ ., labeller = labeller(layer = function(x) gsub("_", " ", x))) +
  labs(x = "Uso Agropecuario", y = "Porcentaje (%)", fill = "Cambio temporal") +
  theme_minimal() +
  theme(
    text = element_text(size = 0.8 * 11),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 7.5),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

plot_por_piso_mk





write.xlsx(puntos_con_valores2, file = "porcentaje_relacion_temporal.xlsx")



# Guardar el gráfico como un archivo TIFF
ggsave("cambio_temporal_pisos.tiff",plot_por_piso_mk , width = 168, height = 168, units = "mm", dpi = 800)


writeRaster(cor_pval,"raster_correlacion_mk.tif")

#### valores de 2 son pendientes engativas no signficiativas 
### valores de 3 son pendientes positivas nos signficiativas
#### vaores de 4 son pendientes negativas significatrivas 
##### y valores de 5 son pendientes psoitivas significativas 

