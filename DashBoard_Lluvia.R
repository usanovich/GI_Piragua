---
  title: "Prueba Dashboard Lluvia-Nivel"
autor: "Jorge Moncayo"
output: 
  flexdashboard::flex_dashboard:
  orientation: rows
social: menu
source_code: embed
#logo: piragua2.png
---
  
  ```{r setup, include=FALSE}

# Carga inicial de librerías

library(tidyverse)
library(ggplot2)
library(dygraphs)
library(plotly)
library(plyr)
library(xts)
library(flexdashboard)
library(urltools)
library(lubridate)
library(reshape2)
library(skimr)


# Funcion para obtener dataset de estación limnigráfica

get_data_limni = function(estacion, inicio, final){
  url="https://www.piraguacorantioquia.com.co/api/nivel/1017?date_estacion__gte=2020-01-01&date_estacion__lt=2020-01-02&downloadfile"
  url2 = param_set(url, "date_estacion__gte", inicio)
  url3 = param_set(url2, "date_estacion__lt", final)
  gsub("1017", estacion, url3)
}

# Función para obtener dataset de estación de lluvia

get_data = function(estacion, inicio, final){
  url="https://www.piraguacorantioquia.com.co/api/precipitacion/40?date_estacion__gte=2020-11-17&date_estacion__lt=2020-11-23&downloadfile"
  url2 = param_set(url, "date_estacion__gte", inicio)
  url3 = param_set(url2, "date_estacion__lt", final)
  gsub("40", estacion, url3)
}

####################################################################

# Leemos los datos
data_lluvia = read.csv(get_data("82","2019-01-01", "2019-12-31"))

# Eliminamos NA
data_lluvia = na.omit(data_lluvia)

# Convertimos a formato fecha diaria
data_lluvia$fechas = as.Date(data_lluvia$fechas)

# Acumulamos por día

dataAcumDiario = aggregate(data_lluvia["muestra"], by = data_lluvia["fechas"], sum)

# Acumulamos por mes

dataAcumMensual = dataAcumDiario
dataAcumMensual$fechas <- format(as.Date(dataAcumMensual$fechas), "%Y-%m")

dataAcumMensual2 = aggregate(dataAcumMensual["muestra"], 
                             by = dataAcumMensual["fechas"], sum)

dataAcumMensual2$fechas = month(ym(dataAcumMensual2$fechas))

dataAcumMensual2$fechas = month.abb[dataAcumMensual2$fechas]

#### Categorización por mes


# Ordenando por mes

min_month = min(dataAcumDiario$fechas)-day(min(dataAcumDiario$fechas))+1
max_month = max(dataAcumDiario$fechas)-day(max(dataAcumDiario$fechas))+1

dataAcumDiario$group <- factor(format(dataAcumDiario$fechas, "%B"), 
                               levels=format(seq(min_month, max_month,by="month"),
                                             "%B"))

# Cambiando nombre a columnas de acumDiario
colnames(dataAcumDiario) = c("Fecha", "Lluvia", "Mes")


###############################################

# Caudales y niveles

# Carga inicial de datos

limni = read_csv(get_data_limni("1017", "2011-01-01", "2020-11-25"))

# Eliminamos filas con NA's

limni = na.omit(limni)

# Cambiando nombre a columnas de limni

colnames(limni) = c("Fecha", "Muestra", "Nivel", "Caudal")

#limni$Ano <- format(as.Date(limni$Fecha), "%Y")
#head(limni)


limni$Fechas = floor_date(limni$Fecha, "2 hours")

limni2 = limni %>%
  group_by(Fechas) %>%
  dplyr::summarise(mean_Nivel = mean(Nivel), mean_Caudal = mean(Caudal))

limni2$Ano <- format(as.Date(limni2$Fechas), "%Y")

```

Reporte anual de precipitación (Estación 82-Envigado)
=======================================================================
  
  Row
-----------------------------------------------------------------------
  ### Serie temporal año 2019
  
  ```{r}

p = ggplot(dataAcumDiario, aes(x = Fecha, y = Lluvia)) +
  stat_smooth(color='blue') +
  geom_line(colour = "#2391D0") +
  labs(x = "Fecha (2019)", y = "mm de lluvia") 

ggplotly(p)

```

### Diagrama de barras para lluvia acumulada mensualmente en el año 2019

```{r}

p = ggplot(dataAcumMensual2) +
  geom_bar(mapping = aes(x = reorder(fechas, -muestra), 
                         y = muestra, fill = fechas,
                         color = fechas), stat = "identity") +
  labs(x = "Meses", y = "Lluvia acumulada [mm]") +
  scale_x_discrete(limits = c("Nov", "Mar", "Sep", "Oct", "Jul",
                              "Feb", "Jan", "Dec", "Jun", "Aug",
                              "May", "Apr"),
                   labels = c("Nov", "Mar", "Sep", "Oct", "Jul",
                              "Feb", "Ene", "Dec", "Jun", "Ago",
                              "May", "Abr")) +
  theme(legend.position = "left") +
  theme(legend.title=element_blank())

ggplotly(p)

```

Row
-----------------------------------------------------------------------
  
  ### Diagramas de cajas y bigotes de lluvia acumulada diariamente analizada mensualmente para el año 2019
  
  ```{r}

p = ggplot(dataAcumDiario) +
  geom_boxplot(mapping = aes(x = Mes, y = Lluvia, fill = factor(Mes), color = Mes)) +
  labs(x = "Meses", y = "Lluvia acumulada diariamente") +
  theme(legend.position = "right") +
  theme(legend.title=element_blank()) +
  scale_x_discrete(limits = c("enero", "febrero", "marzo", "abril", "mayo",
                              "junio", "julio", "agosto", "septiembre","octubre",
                              "noviembre", "diciembre"),
                   labels = c("Ene", "Feb", "Mar", "Abr", "May",
                              "Jun", "Jul", "Ago", "Sep", "Oct",
                              "Nov", "Dic"))
#theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplotly(p)


```


### Resumen estadístico mensual para el año 2019

A continuación se presenta un resumen estadístico para la estación pluviográfica de interés. 

```{r}

dataAcumDiario %>% 
  dplyr::group_by(Mes) %>%
  skim(Lluvia)

```

Reporte de caudales y niveles
=======================================================================
  
  Row
-----------------------------------------------------------------------
  
  ### Reporte histórico de niveles (Estación limnigráfica 1017-Copacabana)
  
  ```{r}

p = ggplot(limni2, aes(x = Fechas, y = mean_Nivel)) +
  stat_smooth(color='blue') +
  geom_line(colour = "#2391D0", alpha = 0.6) +
  #geom_line(stat = "summary", fun.y = "mean", colour = "#009640") +
  labs(x = "Año", y = "Nivel registrado [m]")

ggplotly(p)

```

### Diagrama de cajas y bigotes para datos históricos de niveles (Estación limnigráfica 1017-Copacabana)

```{r}

p = ggplot(limni2) +
  geom_boxplot(mapping = aes(x = Ano, y = mean_Nivel, color = Ano)) +
  labs(x = "Año", y = "Nivel registrado [m]") +
  theme(legend.position = "right") +
  theme(legend.title=element_blank())

ggplotly(p)

```


Row
-----------------------------------------------------------------------
  
  ### Reporte histórico de caudales (Estación limnigráfica 1017-Copacabana)
  
  ```{r}

p = ggplot(limni2, aes(x = Fechas, y = mean_Caudal)) +
  stat_smooth(color='blue') +
  geom_line(colour = "#2391D0", alpha = 0.6) +
  #geom_line(stat = "summary", fun.y = "mean", colour = "#009640")
  labs(x = "Año", y = "Caudal [m\u00b3/s]")

ggplotly(p)

```

### Diagrama de cajas y bigotes para datos históricos de caudal (Estación limnigráfica 1017-Copacabana)

```{r}

p = ggplot(limni2) +
  geom_boxplot(mapping = aes(x = Ano, y = mean_Caudal, color = Ano)) +
  labs(x = "Año", y = "Caudal [m\u00b3/s]") +
  theme(legend.position = "right") +
  theme(legend.title=element_blank())

ggplotly(p)

```


Reporte de lluvia y caudal semanal 
=======================================================================
  
  **Semana 16 -23 de Noviembre de  2020**
  
  Se evidenció:
  
  **Precipitación** 🌧:
  
  El municipio con la estación que presento más precipitación fue **Campamento - (Oficina Territorial Tahamíes)** obtuvo el registro de acumulado de lluvía  para la semana más alto de toda la Jurisdicción para esta semana con una cantidad de **113 mm**, su promedio histórico está en **41 mm** obteniendo un incremento de 175 % de precipitación bajo el valor del promedio histórico registrado.

El municipio con la estación que presentó más precipitación fue **Guadalupe - (Oficina Territorial Tahamíes)**, obtuvo el registro de acumulado de lluvía más alto para la semana de toda la Jurisdicción para esta semana con una cantidad de **176 mm**, su promedio histórico esta en **224 mm**, pero en esta estación NO sobrepaso el promedio historico semanal de este año.

En general, en la Jurisdicción llovió en promedio **42 mm** respecto a los **36 mm** registrados el año pasado, lo cual significa un incremento del 18%.


**Caudal** 〰 :
  
  El resumen del comporatamiento de los principales puntos de medición de caudal de la Jurisdicción con variaciones entre su registro actual y el promedio histórico



**Monitoreo de Calidad**  💧:
  
  Se realizaron: 40 monitoreos en la jurisdicción distribuidos así:
  
  Citará: 26 fuentes abastecedoras monitoreadas
Cartama: 3 fuentes abastecedoras monitoreadas  

Fuentes Instrumentadas: 11 ( fuente dónde tenemos medición de caudal) 