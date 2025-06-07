# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

# Fijo el dataset
attach(datos)

colores <- c(
  "#2A3990",
  "#7890CD",
  "#7126CC",
  "#2059CC"
)

# --------------------------------------------
# Gráfica para analisis bivariado
# --------------------------------------------
# Grafica: Reelacion entre el hacinamiento y el acceso a servicios

ggplot(datos, aes(x = datos$"posee_acceso_completo", y = datos$"indice_hacinamiento")) +
  geom_boxplot(fill = colores[4], alpha = 0.7) +
  labs(
    x = "Acceso a servicios básicos",
    y = "Nivel de hacinamiento",
    title = "Distribución del hacinamiento según acceso a servicios",
    caption = "Datos de La Poderosa - 2020"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

# Grafica: Reelacion entre el hacinamiento y la cantidad de personas por vivenda
ggplot(datos, aes(x = datos$"personas_por_vivienda", y = datos$"indice_hacinamiento")) +
  geom_point(color = colores[1], size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = colores[3], linetype = "dashed") +
  labs(
    title = "Relación entre Número de Personas por Vivienda e Índice de Hacinamiento",
    x     = "Personas por Vivienda",
    y     = "Índice de Hacinamiento",
    caption = "Datos de La Poderosa - 2020"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text    = element_text(size = 12),
    axis.title   = element_text(size = 12)
  )
