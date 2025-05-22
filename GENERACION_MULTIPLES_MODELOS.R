NDVI_CLIMA2 <- read.csv("C:/PROYECTOS/TESIS_GABY-20231125T121206Z-001/TESIS_GABY2/CODIGO/R/MODIS_DATA/NDVI_CLIMA2.csv", stringsAsFactors=TRUE)

library(ggplot2)
library(dplyr)
library(mgcv)




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
  slice_sample(n = 2000) 

muestra_para_modelo


grafica_year = muestra_para_modelo %>%
  ggplot(aes(x = year, y = valor_NDVI, color = Epoca)) +
  geom_smooth(method = "loess", se = T) + # Línea de tendencia
  facet_grid(USO_AGROP ~ layer ) +
  labs(title = "Evolución Temporal de NDVI por Uso Agropecuario, Layer y Época",
       x = "Año",
       y = "Valor NDVI") +
  theme_minimal()
grafica_year

library(parallel)

modelo_gam6 = bam(valor_NDVI ~ USO_AGROP + layer + Epoca + 
                    s(month,  by = interaction(USO_AGROP, layer)) + 
                    s(year,by = interaction(USO_AGROP, layer, Epoca)) + 
                    s(valor_TEMP,k=2, by = interaction(USO_AGROP, layer, Epoca)) + 
                    s(valor_PRECI,k=2,  by = interaction(USO_AGROP, layer, Epoca)),
                  data = muestra_para_modelo, 
                  nthreads = detectCores())  # Usar todos los núcleos disponibles

summary(modelo_gam6)


modelo_gam7 = bam(valor_NDVI ~ USO_AGROP + layer + 
                    s(month,  by = interaction(USO_AGROP, layer)) + 
                    s(year,by = interaction(USO_AGROP, layer)) + 
                    s(valor_TEMP,k=2, by = interaction(USO_AGROP, layer)) + 
                    s(valor_PRECI,k=2,  by = interaction(USO_AGROP, layer)),
                  data = muestra_para_modelo, 
                  nthreads = detectCores())  # Usar todos los núcleos disponibles

summary(modelo_gam7)


modelo_gam8= bam(valor_NDVI ~ USO_AGROP + layer + 
                    s(month,  by = interaction(USO_AGROP, layer)) + 
                    s(year,by = interaction(USO_AGROP, layer, Epoca)) + 
                    s(valor_TEMP,k=2, by = interaction(USO_AGROP, layer)) + 
                    s(valor_PRECI,k=2,  by = interaction(USO_AGROP, layer)),
                  data = muestra_para_modelo, 
                  nthreads = detectCores())  # Usar todos los núcleos disponibles

summary(modelo_gam8)



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

# Aplicar la función a cada modelo y almacenar los resultados
modelos <- list(modelo_gam6,modelo_gam7, modelo_gam8)
stats_list <- lapply(modelos, function(m) extract_stats(m, muestra_gam))

# Convertir la lista de estadísticas en un dataframe
stats_df <- do.call(rbind, stats_list)
stats_df <- as.data.frame(stats_df)

# Dar nombres a las filas si es necesario
row.names(stats_df) <- paste0("modelo_gam", 1:length(modelos))

# Imprimir el dataframe con las estadísticas y la fórmula
stats_df



saveRDS(modelo_gam6, "modelo_gam6.rds")

saveRDS(modelo_gam7, "modelo_gam7.rds")

saveRDS(modelo_gam8, "modelo_gam8.rds")