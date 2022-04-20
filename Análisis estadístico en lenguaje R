Por Vianey Cabello Figueroa
#Es un análisis de una base de datos que contiene información de la capacidad de producción de energía de casi 35 mil plantas de energías renovables y no renovables, en MW al año, también incluye una estimación para el año 2020.
La pregunta de investigación es ¿Las producción de energías renovables a nivel mundial es diferente significativamente de la producción de energías no renovables?.
Para responder se realizó un análisis exploratorio de los datos y posteriormente se hizo una prueba de hipótesis.
Resultó que no existe una diferencia significativa entre la producción de energías renovables y no renovables.
####Abrir base de datos
##Se seleccionó una base de datos de Kaggle.com, la base de datos
##se encuentra en la siguiente liga: https://www.kaggle.com/ramjasmaurya/global-powerplants
setwd("D:/BEDU DATA SCIENCES/R/Checkpoint")
getwd()

read.csv("powerplants.csv") 
df <- read.csv("powerplants.csv")
# Explorar de forma general el dataframe
class(df)
names(df)
View(df) 
#Revisar si hay valores faltantes
lapply(df, is.null)
# Revisar el tipo (class) de nuestras variables
lapply(df, class)

#Análisis exploratorio de datos
summary(df)
#Se muestra que la capacidad en MW va desde mínimo 1 hasta 22500 por planta
#La capacidad de producción de energía mundial es mínimo 1 MW y máximo 22500.00 MW
#GROUP BY
install.packages("dplyr")
library(dplyr)

# Agrupación del gasto por tipo de estimación de generación de energía
df %>% 
  group_by(primary_fuel) %>%
summarise(capacity.in.MW.mean = mean(capacity.in.MW),
            capacity.in.MW.median = median(capacity.in.MW),
            capacity.in.MW.sd = sd(capacity.in.MW))
#En promedio la energía nuclear produce más MW y la que menos produce es “la basura” en #promedio, Storage significa que es energía almacenada
#### HISTOGRAMAS
install.packages("ggplot2")
library(ggplot2)
#Summary de analisis exploratorio de capacidad de MW

ggplot(data = df, aes(x = capacity.in.MW)) + geom_histogram()  +
  xlab("Capacidad total") + ylab("Frecuencia") +
  ggtitle("Capacidad total de producción de energía mundial")

ggplot(data = df) + 
  geom_histogram(aes(x=capacity.in.MW,fill=factor(primary_fuel)),alpha = 0.5)  +
  xlab("Total") + ylab("Frequencia") +
  ggtitle("Total capacidad en MW por tipo de combustible")
#### Boxplots
ggplot(data = df) + geom_boxplot(aes(x=capacity.in.MW))  +
  xlab("") + ggtitle("Total de capacidad en MW")

ggplot(data = df) + geom_boxplot(aes(x=factor(primary_fuel), y=capacity.in.MW)) + 
  coord_flip()  +
  ylab("") + xlab("Tipo de Planta Energética") +
  ggtitle("Capacidad de producción energética")
Pregunta de investigación: ¿Hay diferencias significativas entre la producción de energías renovables y la producción de energías no renovables en MW?
#Evaluar si hay diferencias significativas en la producción energética
#comparando energías renovables vs no renovables, evaluar si la energía renovable
#tiene mas capacidad de producción a nivel mundial
  # Si la energía renovable produce más debería ser mayor su capacidad de producción en MW
  # PASO 1: Planteamiento de hipotesis:
  # H_nula: Renovables.M <= No_Renovables.M
  # H_alt: Renovables.M > No_Renovables.M
  
  # PASO 2: Calcular estadístico de prueba:
  
#Se seleccionaron las energías Renovables y se filtró su producción en MW, eólica, oleaje y mareas, basura, solar, geotérmica, biomasa e hidroléctrica. 

Wind.fil <- filter(df,primary_fuel =="Wind")

WaveandTidal.fil <- filter(df,primary_fuel =="Wave and Tidal")

Waste.fil <- filter(df,primary_fuel =="Waste")

Solar.fil <-  filter(df,primary_fuel =="Solar")

Geothermal.fil <-  filter(df,primary_fuel =="Geothermal")

Biomass.fil <-  filter(df,primary_fuel =="Biomass")

Cogeneration.fil <-  filter(df,primary_fuel =="Cogeneration")

Hydro.fil <-  filter(df,primary_fuel =="Hydro")

#Se seleccionaron las energías No Renovables y se filtró su producción en MW (Petróleo, nuclear, gas, carbón y coque de petróleo.
 
Oil.fil <-  filter(df,primary_fuel =="Oil")

Nuclear.fil <-  filter(df,primary_fuel =="Nuclear")

Gas.fil <-  filter(df,primary_fuel =="Gas")

Coal.fil <-  filter(df,primary_fuel =="Coal")

Petcoke.fil <-  filter(df,primary_fuel =="Petcoke")
 
Renovables.M <- c(mean(Wind.fil$capacity.in.MW),mean(WaveandTidal.fil$capacity.in.MW),
                mean(Waste.fil$capacity.in.MW),mean(Solar.fil$capacity.in.MW),mean(Geothermal.fil$capacity.in.MW),
                mean(Biomass.fil$capacity.in.MW),mean(Cogeneration.fil$capacity.in.MW), mean(Hydro.fil$capacity.in.MW))

No_Renovables.M <- c(mean(Oil.fil$capacity.in.MW), mean(Nuclear.fil$capacity.in.MW), mean(Gas.fil$capacity.in.MW),
                   mean(Coal.fil$capacity.in.MW), mean(Petcoke.fil$capacity.in.MW))

var1 <- c(var(Wind.fil$capacity.in.MW),var(WaveandTidal.fil$capacity.in.MW),
var(Waste.fil$capacity.in.MW),var(Solar.fil$capacity.in.MW),var(Geothermal.fil$capacity.in.MW),
var(Biomass.fil$capacity.in.MW),var(Cogeneration.fil$capacity.in.MW), var(Hydro.fil$capacity.in.MW))
 
var2 <- c(var(Oil.fil$capacity.in.MW), var(Nuclear.fil$capacity.in.MW), var(Gas.fil$capacity.in.MW),
         var(Coal.fil$capacity.in.MW), var(Petcoke.fil$capacity.in.MW))

n1 <- c(length(Wind.fil$capacity.in.MW),length(WaveandTidal.fil$capacity.in.MW),
        length(Waste.fil$capacity.in.MW),length(Solar.fil$capacity.in.MW),length(Geothermal.fil$capacity.in.MW),
        length(Biomass.fil$capacity.in.MW),length(Cogeneration.fil$capacity.in.MW), length(Hydro.fil$capacity.in.MW))

n2 <- c(length(Oil.fil$capacity.in.MW), length(Nuclear.fil$capacity.in.MW), length(Gas.fil$capacity.in.MW),
        length(Coal.fil$capacity.in.MW), length(Petcoke.fil$capacity.in.MW))

t <- (Renovables.M-No_Renovables.M-0)/(sqrt(((n1-1)*var1+(n2-1)*var2)/(n1+n2-2))*sqrt(1/n1+1/n2))
gl <- n1 + n2 - 2


# PASO 3: Calcular P-Value
pvalue <- pt(t, df = gl, lower.tail = FALSE)

pvalue
# PASO 4: Seleccionar nivel de confianza y concluir
# Usualmente se definen niveles de significancia estándar: 0.1, 0.05 o 0.01
# Si Pvalue < significancia, que aquí se utilizará 0.05, se rechaza H_nula, en este caso como el valor de P no es menor
#aunque el nivel de confianza no se rechaza la H_nula, es decir, no hay diferencias
#significativas entre la producción de energías renovables y no renovables.
