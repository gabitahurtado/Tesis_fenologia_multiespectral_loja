
NDVI_CLIMA2 <- read.csv("D:/StarMedia/Users/GABY/Desktop/dats_fnls/COD_FN_TSS/NDVI_CLIMA2.csv", stringsAsFactors=TRUE)

library(ggplot2)
library(dplyr)
library(mgcv)
library(viridis)
library(ggpointdensity)

NDVI_CLIMA2 = na.omit(NDVI_CLIMA2_clean)
# Asumiendo que 'month' es un número de 1 a 12
# Crear un vector con las iniciales de los meses
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

# Convertir 'month' a factor con las iniciales de los meses como niveles
NDVI_CLIMA2$month2 <- factor(NDVI_CLIMA2$month, levels = 1:12, labels = meses)
# Crear una nueva columna para la clasificación
NDVI_CLIMA2$Epoca <- ifelse(NDVI_CLIMA2$month2 %in% c("Ene", "Feb", "Mar", "Abr"), "Lluviosa", "Seca")


NDVI_CLIMA2$Epoca = as.factor(NDVI_CLIMA2$Epoca)
##### muestra 
muestra_para_modelo = NDVI_CLIMA2 %>%
  filter(!is.na(valor_NDVI))%>%
  group_by(layer, USO_AGROP,  Epoca, year, month) %>%
  slice_sample(prop = 0.60) ### SE EXTRAE EL 60% DE LOS DATOS PARA REALZIAR EL MODELO 


muestra_para_modelo



modelo_gam8= bam(valor_NDVI ~ USO_AGROP + layer +
                   s(month,  by = interaction(USO_AGROP, layer)) + 
                   s(year,by = interaction(USO_AGROP, layer, Epoca)) + 
                   s(valor_TEMP,k=2, by = interaction(USO_AGROP, layer)) + 
                   s(valor_PRECI,k=2,  by = interaction(USO_AGROP, layer)),
                 data = muestra_para_modelo, 
                 nthreads = detectCores())  # Usar todos los núcleos disponibles

summary(modelo_gam8)



#gam.check(modelo_gam8)



muestra_gam = NDVI_CLIMA2 %>%
  filter(!is.na(valor_NDVI))%>%
  group_by(layer, USO_AGROP,  Epoca, year, month) %>%
  slice_sample(n = 50)


# Modificar la función para incluir la fórmula
extract_stats <- function(model, data) {
  # Calcular predicciones
  predicted <- predict(model, newdata = data)
  
  # Calcular residuos
  residuals <- data$valor_NDVI - predicted
  residuals <- residuals[!is.na(residuals)]  # Elimina los NA
  
  # Calcular RMSE
  RMSE_model <- sqrt(mean(residuals^2))
  
  # Extraer AIC y BIC
  AIC_model <- AIC(model)
  BIC_model <- BIC(model)
  
  # Calcular pseudo-R^2 para GAM
  explained_deviance <- 1 - model$deviance / model$null.deviance
  pseudo_R2 <- explained_deviance
  
  # Obtener la fórmula del modelo como texto
  formula_model <- as.character(formula(model))
  
  return(c(Formula = formula_model, AIC = AIC_model, BIC = BIC_model, RMSE = RMSE_model, Pseudo_R2 = pseudo_R2))
}


######### EXTRAE LOS DATOS QUE NO SE USARON EN EL MODELO PARA CALCULAR EL RMSE

NDVI_VALIDACION  <- NDVI_CLIMA2 %>%
  anti_join(muestra_para_modelo, by = "id")

# Aplicar la función a cada modelo y almacenar los resultados
modelos <- list(modelo_gam6,modelo_gam7, modelo_gam8)
stats_list <- lapply(modelos, function(m) extract_stats(m, NDVI_VALIDACION))

# Convertir la lista de estadísticas en un dataframe
stats_df <- do.call(rbind, stats_list)
stats_df <- as.data.frame(stats_df)

# Dar nombres a las filas si es necesario
row.names(stats_df) <- paste0("modelo_gam", 1:length(modelos))

# Imprimir el dataframe con las estadísticas y la fórmula
stats_df




saveRDS(modelo_gam8, "modelo_gam_final.rds")

#######################
 ## cuadro de comparaciones 


summary(modelo_gam8)

############################ generacion de graficas 





##### muestra PARA GRAFICAR SE SACA UNA MUESTRA APRA GRAFICAR LOS MODELOS, ESTO PARA OBTIMIZAR EL TIEMPO DE PROCESAMIENTO 
muestra_predict = NDVI_CLIMA2 %>%
  filter(!is.na(valor_NDVI))%>%
  group_by(layer, USO_AGROP,  Epoca, year, month) %>%
  slice_sample(n = 600) 

muestra_predict 

# Calcular predicciones con desviaciones estándar
predicciones <- predict(modelo_gam8, muestra_predict, type = "response", se.fit = TRUE)

# Añadir predicciones y desviaciones estándar al nuevo conjunto de datos
muestra_predict$pred_NDVI <- predicciones$fit
muestra_predict$se <- predicciones$se.fit




# Calcular intervalos de confianza
alpha <- 0.05  # Nivel de confianza del 95%
muestra_predict$ci_lower <- muestra_predict$pred_NDVI - qt(1 - alpha / 2, df = modelo_gam6$df.residual) * muestra_predict$se
muestra_predict$ci_upper <- muestra_predict$pred_NDVI + qt(1 - alpha / 2, df = modelo_gam6$df.residual) * muestra_predict$se


# y cambia el orden de sus niveles según lo solicitado
muestra_predict$layer <- factor(muestra_predict$layer, levels =c("Montano_alto", "Montano", "Montano_bajo", "Piemontano", "Tierras_bajas"))




anual = ggplot(muestra_predict, aes(x = year, y = valor_NDVI, color = Epoca)) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, linewidth = 0.4) +
  # geom_ribbon(aes(x =valor_TEMP,  ymin = ci_lower, ymax = ci_upper, color = Epoca),
  #  alpha = 0.2)  +
  facet_grid(layer ~ USO_AGROP, 
             labeller = labeller(layer = function(x) gsub("_", " ", x))) +
  labs(x = "Año", y = "Valor NDVI") +
  scale_color_manual(values = c("Lluviosa" = "black", "Seca" = "#6E6462")) +
  theme_minimal() +
  theme(text = element_text(size = 0.8 * 11),  # Ajustar el tamaño del texto (0.8 * tamaño base)
        plot.title = element_text(face = "bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),  # Agrega el marco a cada panel
        strip.background = element_rect(fill = "white", colour = "black", size = 1)) + 
  ggtitle("NDVI entre los años 2000 - 2019 en Época seca y lluviosa\n en los diferentes cultivos y pisos climáticos")

# Guardar la gráfica
tiff(filename = "anual.TIF", width = 168, height = 200, units = "mm", res = 800)
print(anual)
dev.off()

# Abreviaturas de los meses en español
meses_esp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

# Creación del gráfico
mensual <- ggplot(muestra_predict, aes(x = month, y = valor_NDVI)) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE,  color = "black", linewidth = 0.4) +
  facet_grid(layer ~ USO_AGROP, labeller = labeller(layer = function(x) gsub("_", " ", x))) +
  labs(x = "Mes", y = "Valor NDVI") +
  theme_minimal() +
  theme(
    text = element_text(size = 0.8 * 11),  # Ajustar el tamaño del texto
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7) , # Rotar y hacer más pequeñas las etiquetas del eje X
    panel.border = element_rect(colour = "black", fill=NA, size=1),  # Agrega el marco a cada panel
    strip.background = element_rect(fill = "white", colour = "black", size = 1)) +
  ggtitle("NDVI entre los meses de enero y diciembre en los diferentes cultivos y pisos climáticos") +
  scale_x_continuous(breaks = 1:12, labels = meses_esp, limits = c(1, 12))  # Ajuste de los ejes

# Crear el archivo TIF
tiff(filename = "mensual.TIF", width = 168, height = 200, units = "mm", res = 800)
print(mensual)
dev.off()

temperatura = ggplot(muestra_predict, aes(x = valor_TEMP, y = valor_NDVI) )  +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 2), se = TRUE, color = "black", linewidth = 0.4) +
  facet_grid(layer ~ USO_AGROP, 
             labeller = labeller(layer = function(x) gsub("_", " ", x))) +
  labs(x = "Temperatura (°c) - Promedio mensual", y = "Valor NDVI") +
  theme_minimal() +
  theme(text = element_text(size = 0.8 * 11),  # Ajustar el tamaño del texto (0.8 * tamaño base)
        plot.title = element_text(face = "bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),  # Agrega el marco a cada panel
        strip.background = element_rect(fill = "white", colour = "black", size = 1)) + 
  ggtitle("Relación de temperatura y NDVI en los diferentes cultivos y pisos climáticos")

# Primero, abrimos un nuevo dispositivo gráfico TIF con las dimensiones y resolución deseadas
tiff(filename = "temperatura.TIF", width = 168, height = 200, units = "mm", res = 800)

# Luego, imprimes tu gráfica en el dispositivo
print(temperatura)

# Finalmente, cierras el dispositivo gráfico
dev.off()


# Crear la gráfica
precipitacion = ggplot(muestra_predict, aes(x = valor_PRECI, y = valor_NDVI)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 2), se = TRUE, color = "black", linewidth = 0.4) +
  facet_grid(layer ~ USO_AGROP, 
             labeller = labeller(layer = function(x) gsub("_", " ", x))) +
  labs(x = "Precipitación (mm en un mes)", y = "Valor NDVI") +
  theme_minimal() +
  theme(text = element_text(size = 0.8 * 11),  # Ajustar el tamaño del texto (0.8 * tamaño base)
        plot.title = element_text(face = "bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),  # Agrega el marco a cada panel
        strip.background = element_rect(fill = "white", colour = "black", size = 1)) +  # Personaliza el fondo de los título) +
  ggtitle("Relación de precipitación y NDVI en los diferentes cultivos y pisos climáticos")
  # Agregar recuadros alrededor de cada gráfica
  # Crear un nuevo conjunto de datos con las coordenadas y dimensiones de los paneles

# Primero, abrimos un nuevo dispositivo gráfico TIF con las dimensiones y resolución deseadas
tiff(filename = "precipitacion.TIF", width = 168, height = 200, units = "mm", res = 800)

# Luego, imprimes tu gráfica en el dispositivo
print(precipitacion)

# Finalmente, cierras el dispositivo gráfico
dev.off()


######## CON LOS DATOS QUE SE SACARON PARA VALIDAR EL MODELO (40%) SE  OBSERVO LOS VALROES PREDICHOS Y OBSERVADOS PARA ENTENDR LA RELACION 


predicciones <- predict(modelo_gam8, NDVI_VALIDACION, type = "response", se.fit = F)

# Añadir predicciones y desviaciones estándar al nuevo conjunto de datos
NDVI_VALIDACION$pred_NDVI <- predicciones




NDVI_VALIDACION_sin_na <- na.omit(NDVI_VALIDACION[, c("valor_NDVI", "pred_NDVI")])


# Calcular los errores (diferencias entre los valores observados y predichos)
errores <- NDVI_VALIDACION_sin_na$valor_NDVI -NDVI_VALIDACION_sin_na$pred_NDVI 

# Elevar al cuadrado los errores
errores_cuadrados <- errores^2

# Calcular la media de los errores cuadrados
media_errores_cuadrados <- mean((errores_cuadrados))

# Calcular la raíz cuadrada de la media de los errores cuadrados para obtener el RMSE
rmse_valor <- sqrt(media_errores_cuadrados)

# Imprimir el RMSE
print(rmse_valor)


# Calcular el RMSE con las filas sin NA
rmse_valor <- rmse(NDVI_VALIDACION_sin_na$valor_NDVI , NDVI_VALIDACION_sin_na$pred_NDVI )

# Imprimir el RMSE
print(rmse_valor)

  
# Ajustar el modelo lineal
modelo_lineal <- lm(pred_NDVI ~ valor_NDVI, data = NDVI_VALIDACION  )
summary(modelo_lineal)
# Extraer la pendiente del modelo lineal
pendiente <- coef(modelo_lineal)["valor_NDVI"]

# Crear el gráfico
PREDICCION_REALES =NDVI_VALIDACION  %>%
  group_by(layer, USO_AGROP, Epoca, year, month) %>%
  slice_sample(n = 100) %>%
  ggplot(aes(x =valor_NDVI, y = pred_NDVI)) +
  geom_pointdensity() +   # Añadir puntos con mapa de calor
  scale_color_viridis(name = "Densidad de puntos") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black") + # Añadir línea de regresión lineal
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "dashed") +  # Añadir línea uno a uno
  labs(x = "Valore observado", y = "Valor predicho") +
  theme_minimal() +
  theme(text = element_text(size = 0.8 * 11),  # Ajustar el tamaño del texto
        plot.title = element_text(face = "bold")) +
  ggtitle("Comparación de Valores Predichos y Observados de NDVI") +
  annotate("text", x = Inf, y = Inf, label = paste("Pendiente:", round(pendiente, 1)), 
           hjust = 1.1, vjust = 1.1, size = 3, color = "black")


# Primero, abrimos un nuevo dispositivo gráfico TIF con las dimensiones y resolución deseadas
tiff(filename = "predicciones.TIF", width = 168, height = 168, units = "mm", res = 800)


print(PREDICCION_REALES)

 
dev.off()





# Crear la gráfica
porueba = ggplot(muestra_predict, aes(x = valor_PRECI, y = valor_NDVI)) +
  geom_smooth(method = "bam", formula = y ~ s(x, k = 2), se = TRUE, color = "black", linewidth = 0.4) +
  facet_grid(layer ~ USO_AGROP, 
             labeller = labeller(layer = function(x) gsub("_", " ", x))) +
  labs(x = "Precipitación (mm en un mes)", y = "Valor NDVI") +
  theme_minimal() +
  theme(text = element_text(size = 0.8 * 11),  # Ajustar el tamaño del texto (0.8 * tamaño base)
        plot.title = element_text(face = "bold")) +
  ggtitle("Relación de precipitación y NDVI en los diferentes cultivos y pisos climáticos")

# Primero, abrimos un nuevo dispositivo gráfico TIF con las dimensiones y resolución deseadas
tiff(filename = "prueban.TIF", width = 168, height = 200, units = "mm", res = 800)

# Luego, imprimes tu gráfica en el dispositivo
print(precipitacion)
dev.off()
