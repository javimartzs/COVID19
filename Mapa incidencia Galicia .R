#CARGAMOS LIBRERIAS QUE VAMOS A USAR
library(tidyverse)
library(ggplot2)
library(sf)
library(mapSpain)
library(viridis)
library(viridisLite)
library(animation)
library(gganimate)

#CARGAMOS LOS DATOS DE INCIDENCIA DEL GITHUB DE @LIPIDO

ultimo <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210125_incidencia_14d_municipios.csv"))
d24f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210124_incidencia_14d_municipios.csv"))
d23f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210123_incidencia_14d_municipios.csv"))
d22f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210122_incidencia_14d_municipios.csv"))
d21f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210121_incidencia_14d_municipios.csv"))
d20f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210120_incidencia_14d_municipios.csv"))
d19f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210119_incidencia_14d_municipios.csv"))
d18f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210118_incidencia_14d_municipios.csv"))
d17f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210117_incidencia_14d_municipios.csv"))
d16f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210116_incidencia_14d_municipios.csv"))
d15f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210115_incidencia_14d_municipios.csv"))
d14f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210114_incidencia_14d_municipios.csv"))
d13f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210113_incidencia_14d_municipios.csv"))
d12f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210112_incidencia_14d_municipios.csv"))
d11f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210111_incidencia_14d_municipios.csv"))
d10f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210110_incidencia_14d_municipios.csv"))
d9f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210109_incidencia_14d_municipios.csv"))
d8f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210108_incidencia_14d_municipios.csv"))
d7f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210107_incidencia_14d_municipios.csv"))
d6f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210106_incidencia_14d_municipios.csv"))
d5f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210105_incidencia_14d_municipios.csv"))
d4f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210104_incidencia_14d_municipios.csv"))
d3f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210103_incidencia_14d_municipios.csv"))
d2f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210102_incidencia_14d_municipios.csv"))
d1f <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/incidencia-municipios/20210101_incidencia_14d_municipios.csv"))


ultimo <- ultimo %>% full_join(d24f) %>% full_join(d23f) %>% full_join(d22f) %>% full_join(d21f) %>% full_join(d20f) %>% 
  full_join(d19f) %>% full_join(d18f)%>% full_join(d17f)%>% full_join(d16f)%>% full_join(d15f)%>% full_join(d14f)%>% 
  full_join(d13f)%>% full_join(d12f)%>% full_join(d11f)%>% full_join(d10f)%>% full_join(d9f)%>% full_join(d8f) %>% 
  full_join(d7f)%>% full_join(d6f)%>% full_join(d5f)%>% full_join(d4f)%>% full_join(d3f)%>% full_join(d2f)%>% 
  full_join(d1f)

#CRARGAMOS MAPA MUNICIPAL GALICIA DEL PAQUETE MAPSPAIN
Galicia.sf <- esp_get_munic(region="Galicia")

#UNIFICAMOS LAU_CODE DE LOS MUNICIPIOS
ultimo <- ultimo %>% dplyr:: mutate(LAU_CODE = codigo_municipio)
ultimo$LAU_CODE <- as.numeric(ultimo$LAU_CODE)
Galicia.sf$LAU_CODE <- as.numeric(Galicia.sf$LAU_CODE)
#Galicia.sf$fecha <- as.Date(Galicia.sf$fecha)

#UNIMOS LOS DOS ARCHIVS CON LEF_JOIN
Galicia.sf <- Galicia.sf %>% left_join(ultimo)

#ELIMINAMOS VARIABLES QUE NO USAMOS
Galicia.sf$name <- NULL
Galicia.sf$casos_14d <- NULL
Galicia.sf$casos_14d_min <- NULL
Galicia.sf$casos_14d_max <- NULL
Galicia.sf$IA14_min <- NULL
Galicia.sf$IA14_max <- NULL
Galicia.sf$name <- NULL
Galicia.sf$codigo_municipio <- NULL
Galicia.sf$codauto <- NULL

#CONVERTIMOS LOS N.A EN CEROS
Galicia.sf$IA14[is.na(Galicia.sf$IA14)] <-0

#GRAFICAMOS EL MAPA
Galicia.sf %>% 
ggplot() + geom_sf(data=Galicia.sf, 
                   aes(fill=IA14), 
                   color= "grey", 
                   size=.1) + 
  
#USAMOS LA ESCALA DE COLORES VIRIDIS
  scale_fill_viridis(name= "IA a 14 Dias", 
                     direction = -1, 
                     option = "A",
                     limits= c(0, 2500),
                     alpha = .95,
                     begin = 0,
                     end = 1,
                     guide = guide_legend(reverse = T))+

#AÑADIMOS LOS TITULOS
  labs(title=" IA14 dias en Galicia a {frame_time}",
       caption = "Javier Martinez (@javimartzs)  -  Datos: @Lipido")+
 
  #USAMOS THEME PARA EDITAR EL EXTERIOR DEL MAPA 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.7, face = "bold", color = "black"),
        plot.subtitle = element_text(size = 15, hjust = .7, color = "grey40"),
        plot.caption = element_text(size = 10, hjust = 0.7, color = "grey40"),
        legend.text.align = 0)+
  
  #AÑADIMOS LA TRANSICION PARA PODER ANIMARLA
  transition_states( fecha, transition_length = 1, state_length = 0)+
  transition_time(fecha)

animate(animacion, fps = 13, width = 750, height = 450)
anim_save("bumbas.gif")


