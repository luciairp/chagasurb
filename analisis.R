datos <- read.csv("SantaRosa.csv", header = T)
library(tidyverse)
datos <- read_csv("SantaRosa.csv", col_types = cols(
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

datos_ord <- datos %>% 
  filter(estadoviv == "Evaluada" | estadoviv == "Evaluada y Rociada/tratada") %>% 
  mutate(
    numTitot = numidad + numidn + numpda + numpdn,
    numTiID = numidad + numidn,
    numTiPD = numpda + numpdn
  ) %>%
  filter(!is.na(numTitot)) %>% 
  dplyr::select(-fecharecol, -lat, -lon, -estadoviv, -`...4`, -`...7`)

library(MASS)#funcion fitdistr
ggplot(datos_ord, aes(numTitot))+
  geom_histogram()

hist(datos_ord$numTitot,xlim=c(0,10),breaks = 100)#grafico histograma
fitdistr(datos_ord$numTitot,"Negative Binomial")
fitdistr(datos_ord$numTitot,"Poisson")
hist(rpois(n=1000,lambda=0.806),xlim=c(0,10))
hist(rnbinom(n=1000,mu=0.806,size=0.0237),xlim=c(0,15),breaks = 50)

NB.1<-glm.nb(datos_ord$numTitot ~ datos_ord$numperro)
summary(NB.1) # Theta:0.03047
plot(NB.1)


library(MuMIn)

mod.NB.1<-glmer.nb(datos_ord$numTitot~datos_ord$numperro+(1|datos_ord$idviv),data=datos_ord,na.action=na.fail)
summary(mod.NB.1)

model.set1<-dredge(mod.NB.1) # el default es AICc
model.set1
MMI1<-model.avg(model.set1,beta=F,rank=AICc)
summary(MMI1) 
coef(MMI1)
confint(MMI1)


# análisis de regresión logística -----------------------------------------

# Identificación de variables relevantes
# Perros, gallinas, techo y pared

