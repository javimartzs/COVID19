#CARGAMOS LIBRERIAS----
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gameofthrones)
library(viridis)
library(viridisLite)
library(scales)
library(plyr)
#LIMPIAR ENTORNO

#CARGAMOS DATAFRAME
Incidencia <- read.csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210120_incidencia_14d_municipios.csv"))


#INCIDENCIA PROVINCIA A CORUÑA
Incidencia <- Incidencia %>% filter(codigo_municipio < 27000, IA14 > 25)
ggplot(Incidencia, aes(x=IA14, y=reorder(municipio, IA14), fill=IA14))+
  geom_bar(stat = "identity", width = 0.7)+
  geom_vline(xintercept= c(500,1000,1500), color= "white")+
  guides(fill= FALSE)+
  scale_fill_viridis(direction = -1, option = "A")+
  scale_x_continuous(breaks = c(500,1000,1500,2000,2500))+
  ylab("")+
  xlab("")+
  ggtitle("Incidencia Acumulada a 14 Dias por Municipios, Provincia de A Coruña",
          subtitle = "Actualizado a X-01-2021")+
  labs(caption="Javier Martinez (@javimartzs) Datos: @lipido")


#INCIDENCIA PROVINCIA PONTEVEDRA
Incidencia <- Incidencia %>% filter(codigo_municipio > 36000, IA14 > 25)
ggplot(Incidencia, aes(x=IA14, y=reorder(municipio, IA14), fill=IA14))+
  geom_bar(stat = "identity", width = 0.7)+
  geom_vline(xintercept= c(250,500,750,1000,1250,1500,1750), color= "white")+
  guides(fill= FALSE)+
  scale_fill_viridis(direction = -1, option = "A")+
  scale_x_continuous(breaks = c(500,1000,1500,2000,2500))+
  ylab("")+
  xlab("")+
  ggtitle("Incidencia Acumulada a 14 Dias por Municipios, Provincia de Pontevedra",
          subtitle = "Actualizado a X-01-2021")+
  labs(caption="Javier Martinez (@javimartzs) Datos: @lipido")


