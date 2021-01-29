#CARGAMOS LIBRERIAS
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gameofthrones)
library(viridis)
library(viridisLite)

#CARGAMOS LOS DATOS DE GALICIA
galicia <- read.csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/galicia.csv"))

#TRANSFORMAMOS LA FECHA EN AS.DATE
galicia$Fecha <- as.Date(galicia$Fecha)

#CREAMOS LA VARIABLE DIFERENCIA DE CASOS ACUMULADOS
galicia <- galicia %>% mutate(diferencia = Galicia.casos.acum - lag(Galicia.casos.acum, 1))
galicia$diferencia[1] <- galicia$diferencia[2]

#GRAFICAMOS CON GGPLOT BARRAS Y SMOOTH
galicia %>% 
  ggplot(mapping = aes(x=Fecha, y=diferencia, fill=diferencia))+
  geom_bar(stat = "identity")+

  #EDITAMS EL GRAFICO: LEYENDA, COLORES Y ETIQUETAS
  guides(fill=FALSE)+
  scale_fill_viridis(option="D")+
  xlab("")+
  ylab("")+
  scale_x_date(date_breaks = "30 days",
               date_labels = c("%b-%d"),
               limits = as.Date(c('2020-03-09','2021-01-30')))+ #Añadir una dia por encima y por debajo
  
  #INSERTAMOS Y EDITAMOS TITULO, SUBTITULO Y CAPTION
  labs(title = "Casos Diarios y velocidad de crecimiento en Galicia",
          subtitle = "Actualizado a X-01-2021",
          caption = "Javier Martinez (@javimartzs) Datos: @Lipido")+
  
  theme (plot.title = element_text(size = 20, hjust = 0.5, face = "bold", color = "black"),
         plot.subtitle = element_text(size = 15, hjust = .5, color = "grey40"),
         plot.caption = element_text(size = 10, hjust = 0.5, color = "grey40"))

























