#CARGAMOS LIBRERIAS----
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gameofthrones)
library(viridis)
library(viridisLite)
library(scales)

#BORRAR ENVIRONMENT
rm(list = setdiff(ls(), c("")))

#CARGAMOS DATOS DE GITHUB
Coruña.ext <- read_csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/coruna.ext.csv"))
Coruña <- read.csv(url("https://raw.githubusercontent.com/lipido/galicia-covid19/master/coruna.csv"))

#CONVERTIMOS FECHAS CON AS.DATE
Coruña$Fecha <- as.Date(Coruña$Fecha)
Coruña.ext$Fecha <- as.Date(Coruña.ext$Fecha)

#FILTRAMOS POR FECHA
Coruña <- Coruña %>% filter(Fecha >= "2020-06-04")
Coruña.ext <- Coruña.ext %>% filter(Fecha >= "2020-06-04")

#CALCULAMOS VARIACION CASOS ACUMULADOS
Coruña <- Coruña %>% mutate(diferencia = A_Coruña.casos.acum - lag(A_Coruña.casos.acum, 1))
Coruña$diferencia[1] <- Coruña$diferencia[2]

#GRAFICAMOS CON GGPLOT
ggplot(Coruña, aes(x = Fecha, y = diferencia, fill = diferencia)) + 
  geom_bar(stat = "identity") +
  geom_line(aes(x =Coruña.ext$Fecha, y = Coruña.ext$A_Coruña.hospitalizados), size=.9, color="black")+
  geom_line(aes(x=Coruña.ext$Fecha, y = Coruña.ext$A_Coruña.uci), size=.9, color="red")+
  
  #EDITAMS EL GRAFICO: LEYENDA, COLORES Y ETIQUETAS
  guides(fill = FALSE)+
  scale_fill_viridis(option = "D")+ 
  xlab("")+ 
  ylab("")+
  scale_x_date(date_breaks = "15 days", 
               labels = date_format("%b-%d"),
               limits = as.Date(c('2020-06-03','2021-01-30')))+  #Añadir una dia por encima y por debajo
  geom_vline(xintercept=as.numeric(Coruña$Fecha[64]), linetype=5)+
  geom_vline(xintercept=as.numeric(Coruña$Fecha[132]), linetype=5)+
  geom_vline(xintercept=as.numeric(Coruña$Fecha[205]), linetype=5)+
  
  #INSERTAMOS TITULO, SUBTITULO Y CAPTION
  labs(title = "Casos Diarios y Hospitalizados prevalentes Area Sanitaria de A Coruña",
       subtitle = "Actualizado a X-01-2021",
       caption = "Javier Martinez (@javimartzs) Datos: @Lipido")+
  
  #EDITAMOS EL TITULO, SUBTITULO Y CAPTION
  theme(plot.title = element_text(size = 15, hjust = 0.5, color="black"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))+
  theme_minimal()












