# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

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
#  Gráficas para variables cuantitativas
# --------------------------------------------
# Grafica: Personas por vivenda
ggplot(datos, aes(x = datos$"personas_por_vivienda")) +
  geom_bar(fill = colores[1], color = "black") +
  labs(
    title = "Cantidad de personas por Vivienda",
    x = "Número de personas por vivienda",
    caption = "Datos de La Poderosa - 2020"
  ) +
  theme_minimal() +
  theme (
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )


# Grafica: Cantidad de dormitorios
ggplot(datos, aes(x = datos$"cantidad_de_dormitorios")) +
  geom_bar(fill = colores[2], color = "black") +
  labs(
    title = "Cantidad de dormitorios por vivienda",
    x = "Cantidad de dormitorios",
    caption = "Datos de La Poderosa - 2020"
  ) +
  theme_minimal() +
  theme (
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

# --------------------------------------------
# Gráfica para variable cuantitativa continua
# --------------------------------------------
# Grafica: Hacinamiento
ggplot(datos, aes(x = datos$"indice_hacinamiento")) +
  geom_boxplot(fill = colores[3], color = "black", width = 0.3) +
  labs(
    title   = "Índice de hacinamiento en barrios populares",
    x       = "Índice de hacinamiento",
    y       = NULL,
    caption = "Datos de La Poderosa - 2020"
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic", size = 8),
    axis.title.y=element_blank(), #Elimina las etiquetas en el eje Y.
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )


# --------------------------------------------
# Gráficas para variables cualitativas
# --------------------------------------------
# Grafica: Hogares que poseen cada servicio
servicios <- c("posee_acceso_agua", "posee_acceso_cloacas", "posee_acceso_electricidad")
n_hogares <- nrow(datos)

# Calculamos los porcentajes
porcentajes <- sapply(servicios, function(columna) {
  total_posee <- sum(datos[[columna]] == "Posee", na.rm = TRUE)
  (total_posee / n_hogares) * 100
})

# Armamos un dataframe con los datos para ggplot2
df_servicios <- data.frame(
  servicio   = factor(c("Agua", "Electricidad", "Cloaca"),
                      levels = c("Agua", "Electricidad", "Cloaca")),
  porcentaje = as.numeric(porcentajes)
)

ggplot(df_servicios, aes(x = servicio, y = porcentaje, fill = servicio)) +
  geom_col(width = 0.6, fill = colores[4], color = "black") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            vjust = -0.3, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Porcentaje de hogares que poseen cada servicio",
    x     = "Servicio",
    y     = "Porcentaje (%)",
    caption = "Datos de La Poderosa - 2020"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )


# Grafica: Cantidad de hogares que poseen todos los servicios
# Calculamos las frecuencias manualmente
frecuencias_servicios <- as.data.frame(table(datos$"posee_acceso_completo"))
colnames(frecuencias_servicios) <- c("Situacion", "n")

ggplot(frecuencias_servicios, aes(x = "", y = n, fill = Situacion)) +
  scale_fill_manual(values = colores) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_minimal() +
  labs(title = "Porcentaje de hogares que poseen todos los servicios",
       y       = NULL,
       x       = NULL,
       caption = "Datos de La Poderosa - 2020") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )


# Grafica: Cantidad de hogares con acceso electricidad
frecuencias_elec <- as.data.frame(table(datos$"tipo_acceso_electricidad"))
colnames(frecuencias_elec) <- c("Metodo", "n")

ggplot(frecuencias_elec, aes(x = "", y = n, fill = Metodo)) +
  scale_fill_manual(values = colores) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_minimal() +
  labs(title = "Metodos de acceso a la electricidad",
       y       = NULL,
       x       = NULL,
       caption = "Datos de La Poderosa - 2020") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )


# Grafica: Forma de acceso al agua
ggplot(datos, aes(y = datos$"tipo_acceso_agua")) +
  geom_bar(fill = colores[1], color = "black") +
  labs(
    title = "Métodos de acceso al agua potable",
    x = "Cantidad de viviendas",
    y = NULL,
    caption = "Datos de La Poderosa - 2020"
  ) +
  xlim(0, max(table(datos$"tipo_acceso_agua")) * 1.1) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

# Grafica: Forma de acceso a cloacas
# Armamos un dataframe con los datos para ggplot2
datos_cloacas <- data.frame(
  satisfaccion = factor(
    datos$"tipo_acceso_cloacas",
    levels = c("No sabe", "Desagüe a red cloacal informal/vecinal", "A pozo negro/ciego", "A cámara séptica", "Desagüe a red cloacal pública"), # Definir el orden
    ordered = TRUE
  )
)

ggplot(datos_cloacas, aes(y = satisfaccion)) +
  geom_bar(fill = colores[1], color = "black") +
  labs(title = "Métodos de remoción de aguas residuales",
       x = "Cantidad de viviendas",
       y = NULL,
       caption = "Datos de La Poderosa - 2020") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

