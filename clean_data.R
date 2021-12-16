## Script- Data Challenge - Limpieza de datos

library(readr)
library(dplyr)

# Importar datos
bd_encuestas <- read_csv("~/Maestría/Materias/Econometría Aplicada I/Teoría/Data challenge/resultados_encuestas.csv")

# Eliminar typos edad
bd_clean <- bd_encuestas %>% 
  filter(edad < 85 & edad > 12)

# Convertir variables string en factores
bd_clean <- transform(bd_clean, sexo = factor(sexo, 
                           levels = c("Hombre", "Mujer", "Otro"),
                           labels = c(0, 1, 2)))

bd_clean <- transform(bd_clean, educ = factor(educ, 
                                              levels = c("Primaria", "Secundaria", "Preparatoria","Licenciatura","Posgrado"),
                                              labels = c(0,1,2,3,4)))

bd_clean <- transform(bd_clean, trabajo_presencial = factor(trabajo_presencial, 
                                               levels = c("No", "Si"),
                                               labels = c(0,1)))

bd_clean <- transform(bd_clean, comparte_dom_60 = factor(comparte_dom_60, 
                                                            levels = c("No", "Si"),
                                                            labels = c(0,1)))

bd_clean <- transform(bd_clean, salud = factor(salud, 
                                              levels = c("Muy mala", "Mala", "Ni mala ni buena","Buena","Muy buena"),
                                              labels = c(0,1,2,3,4)))


bd_clean <- transform(bd_clean, vacunacion = factor(vacunacion, 
                                               levels = c("No","Si", "No me he vacunado"),
                                               labels = c(0,1,2)))

bd_clean <- transform(bd_clean, contagio = factor(contagio, 
                                                    levels = c("No","Si"),
                                                    labels = c(0,1)))

bd_clean <- transform(bd_clean, fam_muerte_covid = factor(fam_muerte_covid, 
                                                  levels = c("No","Si"),
                                                  labels = c(0,1)))

bd_clean <- transform(bd_clean, evento_social_15d = factor(evento_social_15d, 
                                                          levels = c("No","Si"),
                                                          labels = c(0,1)))

bd_clean <- transform(bd_clean, deporte = factor(deporte,
                                                           levels = c("No","Si"),
                                                           labels = c(0,1)))

bd_clean <- transform(bd_clean, opinion = factor(opinion,
                                                 levels = c("No estoy de acuerdo","Indiferente","Estoy de acuerdo"),
                                                 labels = c(1,2,3)))

## Agrupar categorías
### se eliminó categoría otro
bd_clean <- bd_clean %>% 
  filter(sexo != 2) 

### agrupa Primaria (1 obs), Secundaria (9 obs) y Preparatoria (61 obs)
levels(bd_clean$educ) <- c("Preparatoria o menos","Preparatoria o menos","Preparatoria o menos", "Licenciatura", "Posgrado")

### agrupa muy mala (0 obs), mala (5 obs) y ni buena ni mala (66)
levels(bd_clean$salud) <- c("Regular o mala", "Regular o mala", "Regular o mala","Buena","Muy buena")
  
summary(bd_clean)

library(foreign)
write.dta(bd_clean,"bd_clean.dta")

