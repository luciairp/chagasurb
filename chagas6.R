# ANÁLISIS DATOS SANTA ROSA 2018 DESDE CHAGAS 6 #

# paquetes a usar
library(tidyverse)
library(ggeffects)
library(ggpubr)
library(oddsratio)

# datos a usar
datos <- read_csv("SantaRosa26-4.csv", col_types = cols(
  estadoviv = col_factor(),
  gallinerosize = col_factor(c("Chico","Mediano", "Grande")),
  corralsize = col_factor(c("Chico","Mediano", "Grande")),
  techocol = col_factor(c("Si","No")),
  paredcol = col_factor(c("Si","No")),
  `eval+ID` = col_factor(),
  `eval+PD` = col_factor(),
  `eval+IDPD` = col_factor(),
  presidad = col_factor(c("Si","No")),
  presidn = col_factor(c("Si","No")),
  prespda = col_factor(c("Si","No")),
  prespdn = col_factor(c("Si","No"))
) )

# armo objeto para analizar
# me quedo con información de positividad de cada vivienda, y caracterización 
viv_trab <- datos %>% 
  filter(estadoviv == "Evaluada" | estadoviv == "Evaluada y Rociada/tratada") %>% 
  mutate(
    latitud = paste(lat,Columna1,sep = "."),
    longitud = paste(lon,Columna3, sep = "."),
    posiTi = case_when(`eval+ID`== 1 ~ 1,
                       `eval+PD`== 1 ~ 1,
                       `eval+IDPD`== 1 ~ 1),
    posiTi = case_when(posiTi == 1 ~ 1,
                       is.na(posiTi) ~ 0),
    where = case_when(`eval+ID`== 1 ~ "domi",
                      `eval+PD`== 1 ~ "peri",
                      `eval+IDPD`== 1 ~ "domiyperi"),
    where = case_when(where == "domi"~ "domi",
                      where == "peri"~ "peri",
                      where == "domiyperi"~ "domiyperi",
                      is.na(where) ~ "no"),
    where.Tg = case_when(idotra == "Si" ~ "domi",
                         pdotra == "Si" ~ "peri"),
    techo = case_when(techocol == "Si" ~ 1,
                      techocol == "No" ~ 0),
    pared = case_when(paredcol == "Si" ~ 1,
                      paredcol =="No" ~ 0)
    ) %>%
  dplyr::select(-fecharecol, -lat, -lon, -Columna1, -Columna3, -LAT2, -LON4, -estadoviv,
                -numidad, -numidn, -numpda, -numpdn, -presidad, -presidn, -prespda, -prespdn,
                -numinsectos, -idpositivo, -idnegativo, -pdnegativo, -pdpositivo, -idotra, -pdotra,
                -gallinerosize, -corralsize, -`eval+ID`, -`eval+PD`, -`eval+IDPD`, -paredcol,
                -techocol) %>% 
  dplyr::select(idviv, posiTi, where, latitud, longitud, everything())


# corroboro que números generales coinciden con Lore
inf_by_where <- viv_trab %>% 
  group_by(where) %>% 
  summarise(
    n = n())

# miro en general qué pasa con las viviendas con Ti respecto a las demás variables
table(viv_trab$numperro,viv_trab$posiTi)
table(viv_trab$numgato,viv_trab$posiTi)
table(viv_trab$numgallinas,viv_trab$posiTi)
table(viv_trab$techo,viv_trab$posiTi)
table(viv_trab$pared,viv_trab$posiTi)
table(viv_trab$where,viv_trab$posiTi)
table(viv_trab$numgallinas,viv_trab$where)
table(viv_trab$techo,viv_trab$where)
table(viv_trab$pared,viv_trab$where)

# modelo con num perro 
mod_perro <- glm(posiTi ~ numperro, data = viv_trab, family = "binomial")

ggplot(data = viv_trab, aes(x = numperro, y = posiTi)) +
  geom_point(aes(color = as.factor(posiTi)), shape = 1) + 
  stat_function(fun = function(x){predict(mod_perro,
                                          newdata = data.frame(numperro = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad default") +
  theme(legend.position = "none")

# modelo con num gallinas
mod_gall <- glm(posiTi ~ numgallinas, data = viv_trab, family = "binomial")

ggplot(data = viv_trab, aes(x = numgallinas, y = posiTi)) +
  geom_point(aes(color = as.factor(posiTi)), shape = 1) + 
  stat_function(fun = function(x){predict(mod_gall,
                                          newdata = data.frame(numgallinas = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad default") +
  theme(legend.position = "none")

dif_residuos <- mod_gall$null.deviance - mod_gall$deviance

# Grados libertad
df <- mod_gall$df.null - mod_gall$df.residual

# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)

paste("Diferencia de residuos:", round(dif_residuos, 4))
anova(mod_gall, test = "Chisq")

library(vcd)
predicciones <- ifelse(test = mod_gall$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(mod_gall$model$posiTi, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))


# modelo con num habitantes
mod_hab <- glm(posiTi ~ totalhabit, data = viv_trab, family = "binomial")

ggplot(data = viv_trab, aes(x = totalhabit, y = posiTi)) +
  geom_point(aes(color = as.factor(posiTi)), shape = 1) + 
  stat_function(fun = function(x){predict(mod_hab,
                                          newdata = data.frame(totalhabit = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad default") +
  theme(legend.position = "none")

# modelo con num gato
mod_gato <- glm(posiTi ~ numgato, data = viv_trab, family = "binomial")
summary(mod_gato)

ggplot(data = viv_trab, aes(x = numgato, y = posiTi)) +
  geom_point(aes(color = as.factor(posiTi)), shape = 1) + 
  stat_function(fun = function(x){predict(mod_gato,
                                          newdata = data.frame(numgato = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad default") +
  theme(legend.position = "none")

# modelos combinando...
mod_anim <- glm(posiTi ~ numgato + numperro + numgallinas, data = viv_trab, family = "binomial")
summary(mod_anim)

library(oddsratio)
or_glm(data = viv_trab, model = mod_anim, 
       incr = list(numgato = 1, numgallinas = 5, numperro = 1))


predicho_gallinas <-  ggpredict(mod_anim, terms=c("numgallinas"),  type="fixed")
predicho_gato <-  ggpredict(mod_anim, terms=c("numgato"),  type="fixed")
predicho_perro <-  ggpredict(mod_anim, terms=c("numperro"),  type="fixed")

#plots de predichos

gallinas <- ggplot(predicho_gallinas, aes(x=x, y=predicted)) +
  geom_point(data = viv_trab, aes(x = numgallinas, y = posiTi)) +
  geom_line(color="blue", lwd=1.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
  xlab("gallinas") +
  ylab("T. infestans probability") +
  theme_classic()

perros <- ggplot(predicho_perro, aes(x=x, y=predicted)) +
  geom_point(data = viv_trab, aes(x = numperro, y = posiTi)) +
  geom_line(color="blue", lwd=1.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
  xlab("perros") +
  ylab("T. infestans probability") +
  theme_classic()

gatos <- ggplot(predicho_gato, aes(x=x, y=predicted)) +
  geom_point(data = viv_trab, aes(x = numgato, y = posiTi)) +
  geom_line(color="blue", lwd=1.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
  xlab("gatos") +
  ylab("T. infestans probability") +
  theme_classic()

#todos los plots juntos (library ggpubr)
ggarrange(gallinas, perros, gatos,
          labels = c("Gallinas", "Perros", "Gatos"), align = "v",
          ncol = 1, nrow = 3)

# modelo con pared
mod_pared <- glm(posiTi ~ pared, data = viv_trab, family = "binomial")

ggplot(data = viv_trab, aes(x = pared, y = posiTi)) +
  geom_point(aes(color = as.factor(posiTi)), shape = 1) + 
  stat_function(fun = function(x){predict(mod_pared,
                                          newdata = data.frame(pared = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad default") +
  theme(legend.position = "none")

# write.csv(viv_trab, "data_cata.csv")
# este archivo uso en análisis espacial

