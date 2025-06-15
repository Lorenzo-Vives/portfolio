# ---- preambulo ----

#limpiamos pantalla y objetos. Luego, hacemos setting de nuestro directorio 

cat("\014") #limpiamos la pantalla
rm(list = ls()) # limpia objetos
library(dplyr)
library(ggplot2)
library(scales)
library(lme4)
library(misty)
library(texreg)
library(ggrepel)
library(tidyr)
library(lattice)
library(gridExtra)
library(ggthemes)


setwd("D:/RSTUDIOWD/papers")

#abrimos la base de datos

nba<- read.csv("NBA_PLAYERS.csv")

#----trabajo de variables----

#eliminamos los na
nba<- nba %>% 
  na.omit()


#desplegamos las variables

colnames(nba)
nba %>% 
count(School) %>% 
  head(10)

nba <- nba %>%
  mutate(School = ifelse(School == "" | is.na(School), "street", School))

#analisis exploratorio:



#transformamos altura a cm y peso a kg

nba<- nba %>% 
  mutate(
    Height_cm = Height * 2.54,
    Weight_kg = Weight * 0.453592,
    total_pts_carrer = PTS * G
    )

nba %>% 
  summarise(
    mean_pts = mean(PTS),
    max_pts = max(PTS),
    sd_pts = sd(PTS),
    avg_height = mean(Height_cm),
    avg_weight = mean(Weight_kg)
  )

#filtramos las posiciones para buscar solo a aquellos jugadores que juegan de pivot


nba %>% 
  count(Position)

nba %>% 
  filter(Position %in% c("['Center', 'Forward']", "['Center']", "['Forward', 'Center']")) %>% 
  group_by(Position) %>% 
  summarise(AVG_PTS = mean(PTS))


#exploramos a nuestros pivot

nba %>% 
  filter(Position %in% c("['Center', 'Forward']", "['Center']", "['Forward', 'Center']"),
         Active == "True") %>%  
  group_by(Position) %>% 
  slice_max(order_by = PTS, n = 10) %>% 
  select(Position, Name, PTS, Active, Height_cm, Weight_kg) %>% 
  arrange(Position, desc(PTS))

#nuestro top 3 por posición 

nba %>% 
  filter(Position %in% c("['Center', 'Forward']", "['Center']", "['Forward', 'Center']"),
         Active == "True") %>%  
  group_by(Position) %>% 
  slice_max(order_by = PTS, n = 3) %>% 
  select(Position, Name, PTS, Active, Height_cm, Weight_kg, G,total_pts_carrer) %>% 
  arrange(Position, desc(PTS))


#modificamos algunas variables de cara a las regresiones

#cambiamos el nombre a la variable G por games
#creamos una variable llamada PTSCENTER que es el promedio de puntos anotados por pivots activos en 2025

nba<- nba %>% 
  mutate(
    games = G,
    PTSCENTER = case_when(
      Position %in% c("['Center', 'Forward']", "['Center']", "['Forward', 'Center']") & Active == "True" ~ PTS
    )
  )

#cambiamos la cat de referencia de Position

nba <- nba %>% 
  mutate(
    Position = relevel(factor(Position), ref = "['Center']")
  )


#creamos un dataset nuuevo para los pivots activos

nba_centers <- nba %>%
  filter(!is.na(PTSCENTER))


#----OLS----

#modelo

m1<- lm(PTSCENTER ~ Position + games +Height_cm + Weight_kg + AST + TRB, data = nba_centers )
screenreg(m1)

nba_centers$fittedvalues <- predict(m1)

#predicciones

nba_centers %>% 
  select(Name, PTS, fittedvalues, Position) %>% 
  slice_max(order_by = fittedvalues, n = 10) 


nba_centers %>%
  ggplot(aes(x = fittedvalues, y = PTS)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Comparación entre puntos reales y predichos",
    x = "Puntos predichos (fittedvalues)",
    y = "Puntos reales (PTS)"
  ) +
  theme_bw()





#----LOGIT----

#promedio de rebotes

nba_centers %>% 
  summarise(
    avg_rb = mean(TRB),
    max = max(TRB),
    min = min(TRB)
  )

#creamos variable dummy, donde si promedian menos de 6 rebotes por partido es 0
#y mayor o igual a 6 rebotes por partido es 1

nba_centers <- nba_centers %>% 
  mutate(
    rb_dummy = case_when(
      TRB >= 6 ~ 1,
      TRB < 6 ~ 0
    )
  )

#modelo logit

m2<- glm(rb_dummy ~  PTSCENTER + Position + games +Height_cm + Weight_kg + AST, data = nba_centers,
         family = binomial (link = "logit") )
screenreg(m2)

 
#----multinivel----


nba<- nba %>% 
  mutate(
    CENTERS = Position %in% c("['Center', 'Forward']", "['Center']", "['Forward', 'Center']")
    )
  



#escuelas más repetidas

nba %>% 
  count(School, sort = TRUE) %>% 
  slice_max(n, n = 10) 


top5_schools <- nba %>%
  count(School, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  pull(School)
  
 

nba_top5_schools <- nba %>% 
  filter(School %in% top5_schools) 

nba_top5_schools %>% 
  count(School, sort = T)

#en esta parte, vamos a abrir la muestra a todas las posiciones, no solo pivots. También se incluyen jugadores inactivos
# se escogen las 5 escuelas más con más jugadores. Se incluye aquellos que no tienen escuela (street)



#centramos las variables a la gran media

nba_top5_schools$games_cgm <- center(nba_top5_schools$games, type = "CGM")
nba_top5_schools$Height_cm_cgm <- center(nba_top5_schools$Height_cm, type = "CGM")
nba_top5_schools$Weight_kg_cgm <- center(nba_top5_schools$Weight_kg, type = "CGM")
nba_top5_schools$AST_cgm <- center(nba_top5_schools$AST, type = "CGM")
nba_top5_schools$TRB_cgm <- center(nba_top5_schools$TRB, type = "CGM")


mnulo<- lmer(PTS ~ 1 + (1|School), data = nba_top5_schools)

m3<- lmer(PTS ~ Position + games_cgm + Height_cm_cgm + Weight_kg_cgm + AST_cgm + TRB_cgm + (1|School)  , data = nba_top5_schools)


m4<- lmer(PTS ~ Position + games_cgm + Height_cm_cgm + Weight_kg_cgm + AST_cgm + TRB_cgm + (TRB_cgm|School)  , data = nba_top5_schools)
screenreg(list(mnulo,m3, m4), single.row = T, custom.model.names = c("model 0", "model 3", "model 4") )

#ICC

ICC = 0.93/(0.93+29.32) * 100
print(ICC)

#anova
anova(m3,m4)

#grafico


qqmath(ranef(mnulo, condVar = TRUE))





ggplot(nba_top5_schools, aes(x = TRB, y = PTS, color = School)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  facet_wrap(~ School) +
  labs(
    title = "Relación TRB - PTS por School con líneas de regresión",
    x = "Rebotes promedio",
    y = "Puntos promedio",
    color = "School"
  ) +
  theme_bw()




# 1. Rangos TRB por School (variable original sin centrar)
trb_ranges <- nba_top5_schools %>%
  group_by(School) %>%
  summarise(
    min_TRB = min(TRB, na.rm = TRUE),
    max_TRB = max(TRB, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Crear secuencia TRB para predecir (50 puntos entre min y max)
new_data <- trb_ranges %>%
  rowwise() %>%
  mutate(TRB = list(seq(min_TRB, max_TRB, length.out = 50))) %>%
  unnest(cols = c(TRB)) %>%  # Para tidyr >= 1.0.0 usar cols= c()
  ungroup()

# 3. Calcular promedios por School y Position para las demás variables originales sin centrar
promedios <- nba_top5_schools %>%
  group_by(School, Position) %>%
  summarise(
    games = mean(games, na.rm = TRUE),
    Height_cm = mean(Height_cm, na.rm = TRUE),
    Weight_kg = mean(Weight_kg, na.rm = TRUE),
    AST = mean(AST, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Calcular posición modal (más frecuente) por School
pos_mode <- nba_top5_schools %>%
  count(School, Position) %>%
  group_by(School) %>%
  slice_max(n, n = 1) %>%
  select(School, Position) %>%
  ungroup()

# 5. Unir posición modal con promedios
promedios_mod <- left_join(pos_mode, promedios, by = c("School", "Position"))

# 6. Unir con new_data por School
new_data <- left_join(new_data, promedios_mod, by = "School")

# 7. Centrar variables continuas en new_data con la gran media de nba_top5_schools
# Calcular medias globales (usadas para centrar)
medias_globales <- nba_top5_schools %>%
  summarise(
    games_m = mean(games, na.rm = TRUE),
    Height_cm_m = mean(Height_cm, na.rm = TRUE),
    Weight_kg_m = mean(Weight_kg, na.rm = TRUE),
    AST_m = mean(AST, na.rm = TRUE),
    TRB_m = mean(TRB, na.rm = TRUE)
  )

# Añadir variables centradas
new_data <- new_data %>%
  mutate(
    games_cgm = games - medias_globales$games_m,
    Height_cm_cgm = Height_cm - medias_globales$Height_cm_m,
    Weight_kg_cgm = Weight_kg - medias_globales$Weight_kg_m,
    AST_cgm = AST - medias_globales$AST_m,
    TRB_cgm = TRB - medias_globales$TRB_m
  )

# 8. Asegurar que Position es factor con niveles correctos
new_data$Position <- factor(new_data$Position, levels = levels(nba_top5_schools$Position))

# 9. Predecir PTS con el modelo m4 usando efectos aleatorios (por School)
new_data$PTS_pred <- predict(m4, newdata = new_data, re.form = NULL)

# 10. Gráfico
ggplot() +
  geom_point(data = nba_top5_schools, aes(x = TRB, y = PTS, color = School), alpha = 0.5) +
  geom_line(data = new_data, aes(x = TRB, y = PTS_pred, color = School), size = 1) +
  facet_wrap(~ School) +
  labs(
    title = "Predicción de PTS según TRB y otras variables, por School",
    x = "Rebotes Totales (TRB)",
    y = "Puntos (PTS)",
    color = "School"
  ) +
  theme_bw()




interceptos<- ranef(m4) %>% print()

interceptos1<- ranef(m3) %>% print()



















