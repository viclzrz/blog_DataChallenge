# =============================================================================
# Autores 	: Karina Gomez, Vicente, Miriam, Raymundo, Guillermo, Ricardo
# Materia 	: Econometria Aplicada - Data Challenge
# Objetivo	: Limpieza base de datos
# Output    : Tabla de estadísticas descriptivas
#             Figura 1 de est. desc
#             Figura 2 de est. desc
#             Figura 3 de est. desc
# Modificado: 14/dic/21
# =============================================================================

# Install packages ------------------------------------------------------------
rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, foreign, gplots, Hmisc, readxl, ggpubr,
               hrbrthemes, viridis, rgdal, raster, rgeos, sp, dplyr,
               scales, maptools, GISTools, RColorBrewer, classInt, ggspatial,
               data.table, dismo, xtable, readstata13, reshape2, sf,
               grid, gridExtra, gtable, plyr, stringr, pBrackets,zeallot,
               plotly, wesanderson, rvest, httr, ggthemes, haven, kableExtra)

install.packages("devtools") 
devtools::install_github("BlakeRMills/MetBrewer")

# Set directory 
if(Sys.info()["user"]=="karinagomez") {
  setwd("/Users/karinagomez/Dropbox/Econometria Aplicada I/Data Challenge/")
}
# -----------------------------------------------------------------------------

# Llama base de datos
base <- read_dta("bd_clean.dta")

# %%%% 1. Tabla de estadisticas descriptivas %%%%
# Define nuevas variables
## Mujer
base_ed <- base %>% mutate(mujer = if_else(sexo == 1, 1, 0))

## Escolaridad
base_ed <- base_ed %>% mutate(educ = if_else(if_else(educ == 1, 9,
                                if_else(educ == 2, 12,16))))
## Salud
base_ed <- base_ed %>% mutate(salud = as.integer(as.character(salud)))

## Vacunacion
base <- base %>% 
  mutate(vacunacion_c = if_else(vacunacion == 1, 1, 0),
         vacunacion_i = if_else(vacunacion == 0, 1, 0),
         n_vacunacion = if_else(vacunacion == 2, 1, 0))

# Prepara para desc. stats (codigo vicente)
base_ed <- base %>% 
  mutate(trabajo_presencial = as.integer(as.character(trabajo_presencial)),
         comparte_dom_60 = as.integer(as.character(comparte_dom_60)),
         contagio = as.integer(as.character(contagio)),
         fam_muerte_covid = as.integer(as.character(fam_muerte_covid)),
         evento_social_15d = as.integer(as.character(evento_social_15d)),
         deporte = as.integer(as.character(deporte)))
base_ed <- base_ed %>% 
  select(edad, mujer, educ, trabajo_presencial, comparte_dom,
         comparte_dom_60, salud, vacunacion_c, vacunacion_i,
         n_vacunacion, contagio, prob_contagio_3m, fam_muerte_covid,
         recomendaciones, impacto_soc, impacto_hogar, evento_social_15d,
         deporte)

vars <- c("Edad", "Mujer", "Escolaridad (Años)", "Trabajo presencial",
          "# Personas en domicilio", "Comparte domicilio (>60 años)", 
          "Percepción estado de Salud", "Vacunación completa", "Vacunación incompleta",
          "No vacunado", "Contagiado", "Percepción Pr contagio", 
          "Familiar muerto p/COVID", "Calificación seguimiento recomendaciones",
          "Preocupación impacto rel. sociales", "Preocupación impacto estrés hogar",
          "Asistencia a evento social", "Aficionada a deporte")

vars <- data.frame(Variable = vars)

ed1 <- base_ed %>%
  summarise(across(.fns = ~ mean(.x, na.rm = T))) %>% 
  pivot_longer(cols = edad:deporte,
               names_to = "var",
               values_to = "Media") 
ed2 <- base_ed %>% 
  summarise(across(.fns = ~ sd(.x, na.rm = T))) %>% 
  pivot_longer(cols = edad:deporte,
               names_to = "var",
               values_to = "Desviación Estándar")
ed3 <- base_ed %>% 
  summarise(across(.fns = ~ sum(!is.na(.x)))) %>% 
  pivot_longer(cols = edad:deporte,
               names_to = "var",
               values_to = "Observaciones") 

ed <- ed1 %>% left_join(ed2, by = "var") %>% left_join(ed3, by = "var")
cbind(vars, ed) %>% select(-c(var)) %>% kbl(digits = 2) %>% kable_minimal()

# %%%%%% Figura 1 de estadisticas descriptivas %%%%%
f1_colors <- c( "#9d9cd5", "#f5bb50")

f1a <- ggplot(base, aes(x=edad, color = as.factor(sexo))) + 
    geom_histogram(aes(y=..count../sum(..count..)), fill="white", bins = 30) + 
    scale_color_manual(labels = c("Mujer", "Hombre"), values=f1_colors) +
    theme_classic() +
    labs(x = "Edad", 
         y = "Porcentaje", 
         colour="Sexo", 
         title = "Edad de encuestados") +
    geom_vline(xintercept = mean(base$edad),linetype = 'dashed', colour = "#062e3d", size = .5)

f1b <- ggplot(base, aes(x=factor(educ))) +
  geom_bar(aes(y=..count../sum(..count..), color = factor(educ)), fill="white", width = .5) +
  theme_classic() +
  coord_flip() +
  labs(x = " ", 
       y = "Porcentaje", 
       title = "Educación de encuestados") +
  scale_x_discrete(labels= c("Preparatoria", "Licenciatura", "Posgrado")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), guide="none")

ggarrange(f1a, f1b,
          ncol = 2, nrow = 1)

# %%%%%% Figura 2 de estadisticas descriptivas %%%%%
ggplot(base, aes(x=comparte_dom)) +
  geom_bar(aes(y=..count../sum(..count..), fill = factor(comparte_dom))) +
  theme_classic() +
  labs(x = " ", 
       y = "Porcentaje", 
       title = "Número de cohabitantes") +
  scale_fill_manual(values = c("#7c668c", "#b08ba5", "#dfbbc8", "#ffc680", "#ffb178"), guide="none")


# %%%%%% Figura 3 de estadisticas descriptivas %%%%%
base.os <- base %>% 
  group_by(vacunacion) %>% 
  summarise(cnt = n()) %>%
  ungroup()%>% 
  arrange(desc(vacunacion)) %>%
  mutate(percentage = round(cnt / sum(cnt), 3)*100,
         lab.pos = cumsum(percentage)-.5*percentage)

f3a <- ggplot(data = base.os, 
       aes(x = 2, y = percentage, fill = factor(vacunacion)))+
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
  theme_void() +
  scale_fill_brewer(palette = "Accent", labels = c("1 dosis", "2 dosis", "Sin vacunar"))+
  xlim(.2,2.5) +
  labs(x = " ", 
       y = " ", 
       fill=" ", 
       title = "Porcentaje de vacunados") +
  theme(plot.title = element_text(hjust = 0.5))

base.es <- base %>% 
  group_by(recomendaciones) %>% 
  summarise(cnt = n()) %>%
  ungroup()%>% 
  arrange(desc(recomendaciones)) %>%
  mutate(percentage = round(cnt / sum(cnt), 3)*100,
         lab.pos = cumsum(percentage)-.5*percentage)

f3b <- ggplot(data = base.es, 
       aes(x = 2, y = percentage, fill = factor(recomendaciones)))+
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
  theme_void() +
  scale_fill_manual(values = rev(brewer.pal(5, "Set2")), labels = c("Nada", "Casi nada", "Más o menos", "Casi completamente", "Completamente"))+
  xlim(.2,2.5) +
  labs(x = " ", 
       y = " ", 
       fill=" ", 
       title = "Cumplimiento de medidas de distanciamiento")+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(f3a, f3b,
          ncol = 2, nrow = 1)
