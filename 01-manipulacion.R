# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(dplyr)

# Fijo el dataset
attach(datos)

######################
# Renombrar columnas #
######################
colnames(datos)[6] <- "personas_por_vivienda"
colnames(datos)[13] <- "cantidad_de_dormitorios"
colnames(datos)[24] <- "tipo_acceso_agua"
colnames(datos)[33] <- "tipo_acceso_cloacas"
colnames(datos)[50] <- "tipo_acceso_electricidad"

#No me gusta dplyr, pero queda mas prolijo. Hacemos esto para acortar las etiquetas
datos$"tipo_acceso_agua" <- recode(datos$"tipo_acceso_agua",
                           "A través de una conexión sin medidor, es decir “informalmente”, sea a través de una conexión directa a la red pública o a través de una conexión indirecta a través de un vecinx “informalmente”" = "De forma informal",
                           "A través de una conexión con medidor a la red pública" = "Con un medidor",
                           "A través de un camión cisterna" = "Con un camion cisterna",
                           "No poseo agua dentro de la vivienda y/o tengo que acarrear desde fuera del terreno en que se ubica mi vivienda" = "No poseo agua en la vivienda")

#Generamos las variables que necesitamos
datos$"indice_hacinamiento" <- (datos$"personas_por_vivienda" / datos$"cantidad_de_dormitorios")


datos$"posee_acceso_agua" <- ifelse(datos$"tipo_acceso_agua" %in%
                                    c("Con un camion cisterna", "No poseeo agua en la vivienda"),
                                    "No posee", "Posee")

datos$"posee_acceso_cloacas" <- ifelse(datos$"tipo_acceso_cloacas" %in%
                                         c("Desagüe a red cloacal informal/vecinal", "No sabe"),
                                       "No posee", "Posee")

datos$"posee_acceso_electricidad" <- ifelse(datos$"tipo_acceso_electricidad" == "No posee conexión a la red eléctrica en la vivienda",
                                            "No posee", "Posee")

datos$"posee_acceso_completo" <- ifelse(datos$"posee_acceso_agua" == "Posee" & datos$"posee_acceso_cloacas" == "Posee" & datos$"posee_acceso_electricidad" == "Posee",
  "Posee todos los servicios",
  "No posee 1 o mas servicios")
