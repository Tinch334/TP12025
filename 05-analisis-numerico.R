# Cargo los paquetes que voy a usar
library(tidyverse)
library(knitr)


# Fijo el dataset
attach(datos)

# ---------------------------
# Definimos las variables que vamos a usar
# ---------------------------
vars_cuantitativas <- c(
  "personas_por_vivienda",
  "cantidad_de_dormitorios",
  "indice_hacinamiento"
)

vars_cualitativas <- c(
  "tipo_acceso_agua",
  "tipo_acceso_cloacas",
  "tipo_acceso_electricidad",
  "posee_acceso_completo"
)

# ---------------------------
# Calculamos la moda
# ---------------------------
calcular_moda <- function(x) {
  x_no_na <- x[!is.na(x)]
  if (length(x_no_na) == 0) return(NA)
  tabla_freq <- table(x_no_na)
  modas <- names(tabla_freq)[tabla_freq == max(tabla_freq)]
  paste(modas, collapse = ", ")
}

# ---------------------------
# Calculamos las tablas de frecuencia
# ---------------------------
resultados_cuant <- list()

for (var in vars_cuantitativas) {
  vec <- datos[[var]]
  vec_no_na <- vec[!is.na(vec)]
  
  # Tabla de frecuencias simples
  freq_simple <- as.data.frame(table(vec_no_na))
  colnames(freq_simple) <- c(var, "Frecuencia")
  
  # Frecuencia relativa y acumulada
  freq_simple <- freq_simple %>%
    mutate(
      `Frecuencia Relativa`          = Frecuencia / sum(Frecuencia),
      `Frecuencia Acumulada`         = cumsum(Frecuencia),
      `Frecuencia Relativa Acumulada` = cumsum(Frecuencia) / sum(Frecuencia)
    )
  
  # Mediana y cuartiles
  mediana  <- median(vec, na.rm = TRUE)
  cuartiles <- quantile(vec, probs = c(0.25, 0.75), na.rm = TRUE)
  
  resultados_cuant[[var]] <- list(
    tabla_frecuencias = freq_simple,
    mediana           = mediana,
    cuartil_1         = cuartiles[[1]],
    cuartil_3         = cuartiles[[2]]
  )
}

# ---------------------------
# Variables cualitativas
# ---------------------------
modas_cualitativas <- tibble(
  Variable = character(),
  Moda     = character()
)

for (var in vars_cualitativas) {
  moda_var <- calcular_moda(datos[[var]])
  modas_cualitativas <- modas_cualitativas %>%
    add_row(Variable = var, Moda = moda_var)
}

# ---------------------------
# Mostramos los resultados
# ---------------------------
# Variables cuantitativas
for (var in names(resultados_cuant)) {
  cat("------------------------------------------------------------\n")
  cat("Variable cuantitativa:", var, "\n\n")
  
  cat("Tabla de frecuencias (simple, relativa y acumuladas):\n")
  print(
    resultados_cuant[[var]]$tabla_frecuencias %>%
      kable(format = "pipe", digits = 4)
  )
  cat("\n")
  cat("Mediana:", resultados_cuant[[var]]$mediana, "\n")
  cat("Primer cuartil (Q1):", resultados_cuant[[var]]$cuartil_1, "\n")
  cat("Tercer cuartil (Q3):", resultados_cuant[[var]]$cuartil_3, "\n\n")
}

# Variables cualitativas
cat("============================================================\n")
cat("Moda de variables cualitativas:\n\n")
print(modas_cualitativas %>% kable(format = "pipe"))
