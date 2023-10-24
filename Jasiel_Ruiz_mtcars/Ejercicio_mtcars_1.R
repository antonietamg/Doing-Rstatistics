

#### ESTADÍSTICA APLICAD CON R  4/10/2023. ####
#### SEMESTRE AGO-DIC 2023.
#### DRA. ANTONIETA MAERTINEZ.
#### SOLUCIONES AL EJERCICIO MTCARS ####
#### ALUMNO : JASIEL RUIZ

# INSTRUCCIONES

# Lee con atencion. Ejecuta las instrucciones y escribe los resultados. Cada
# pregunta debe venir con su codigo y su respuesta escrita como comentario. 
# Guarda todos los archivos que generes pues deberás subirlos a Github.

# 1. Visualiza el dataset mtcars y guardalo como un dataframe llamado "autos" 
#    en el environment.

mtcars

autos= mtcars


# 2. Ve a la pestaña de "Help" y escribe "mtcars" en el buscador. 
#    Lee de que se trata este dataset.


# 3. Grafica con plots tipo línea cada una de las variables. Solamente al ver 
#    estos plots,¿cual de las variables consideras que tiene una mayor variabilidad?
#    Escribe tu respuesta y argumenta por que. (Exporta tus gráficas)

## RESPUESTA

##  Las variables que tienen mayor variabilidad son:
##  Miles per Galon, Displacement, Horse Power,Rear axle ratio,
##  Weight y Quater per mile time, se puede apreciar que no siguen
##  ninguna tendencia o regularidad y tine un rango muy grande o grande.

##  MATRIZ DE DISPERSIÓN ###

plot(autos,main="Matriz de Dispersion", col="red")

### 1.PRIMER INTENTO(IDEA ORIGINAL)

# Declarando un vector con cada una de las variables del dataframe.
variables = c('Miles per gallon','Number of cylinders','Displacement',
              'Horse power','Rear axle ratio','Weight (Lb)',
              'Quater per mile time','Engine','Transmission',
              'Number of foward gears','Number of carburetors')

# Declarando la ruta (en este caso no la utilicé porque ya había establecido
# mi working directory y por default R guarda los archivos en ese
# directorio previamente establecido si necesidad de indicar la 
# ruta de nuevo)
ruta = "C:/Users/jasie/OneDrive/Escritorio/UAEM/Noveno sem/Statistics_R/Jasiel_Ruiz_mtcars/Graficas de Dispersión"

# creando un ciclo para generar todas las imagenes.
for (i in 1:length(variables)){
  png(paste("Dispersion_plot_", i, ".png"))
  plot(autos[,i],col="red",xlab = "Model of the car",
       ylab = variables[i])
  dev.off()
}

# Esto lo hice para probar que en efecto al ingresar a cierta
# posición del vector variables, tomasé el nobre de la vriable indicada
# y eso lo metí en el ciclo para que conforme fuera corriendo el
# contador, la etiqueta de eje y fuese cambiando a la variable que 
# corresponde a dicha posición.
print(variables[1])






### 2. SEGUNDO INTENTO(IDEA MODIFICADA DE PERPLEXITY  AI &
###    CON  LA AYUDA DE LA DRA.)

# Calling ggplot
library(ggplot2)

# Calling tidyverse
library(tidyr)

# Declarando un vector con cada una de las variables del dataframe.
variables = c('Miles per gallon','Number of cylinders','Displacement',
              'Horse power','Rear axle ratio','Weight (Lb)',
              'Quater per mile time','Engine','Transmission',
              'Number of foward gears','Number of carburetors')

# Guardando el data frame mtcars con la etiqueta "autos"
autos= mtcars

# Asegurandome de que en efecto sea un data frame
# (este es fundamental ya que ggplot  y la función 
# "gather" trabajan únicamente con data frames).
autos= as.data.frame(autos)


# Haciendo todo una sola columna con la función gather.
# La info hacercsa de "gather" está en la sig página
# URL: https://mauricioanderson.com/curso-r-tidyr/#:~:text=La%20funci%C3%B3n%20gather%20toma%20m%C3%BAltiples,sino%20valores%20de%20una%20variable.

new_autos=gather(data = autos, key = "Variable",
                 value = "Valor",1:11)

# El data frame new_autos, no tiene una columna con los modelos
# de los autos, es por ello que los agregaremos


# Con la función "unique" seleccionamos los valores unicos
# de la columna "variables" del data frame "new_autos"
# esto nos servirá para ir cambiando el nobre del eje y en cada plot
etiquetas = unique(new_autos$Variable)
etiquetas
# Para poder acceder a los nombres de las filas y columnas de un 
# dataframe utilizamos la función "rownames" & "colnames".

filas=rownames(autos)
filas

# Ahora creamos una columna llamada "Modelos" con los modelos 
# de cada auto repetidos 11 veces para poder unirlos con el data
# frame "new_autos"
Modelos = rep(filas,length(etiquetas))

# Recordemos que podemos unir una columna o fila a un data frame
# con las funciones "cbind" & "rbind" respectivemente.
new_autos=cbind(Modelos,new_autos)


# Create a list of file names for the plots
plot_names <- paste0("plot_", 1:11, ".png")


## Ahora sí, realizamos el ciclo
for (i in 1:length(etiquetas)) {
  # CLAVE: con la función "filter" filtramos y guardamos en el vector "var"
  # todos los datos del dataframe "new_autos" en cada ciclo si y solo si
  # las filas de la columna "Variable" son iguales a 
  # el vaolor que tiene en la posición "i" el vector etiquetas.
  var=dplyr::filter(new_autos,Variable==etiquetas[i])
  # Create plot
  plot_data <- var
  plot <- ggplot(data = plot_data)+ 
    geom_point(aes(x =Modelos,y=Valor, col ='dark blue'))
  
  # Save plot as png
  png(filename = plot_names[i], width = 800, height = 600)
  print(plot)
  #print(plot)
  dev.off()
}



# 4. Grafica las distribuciones de cada variable con hist() y con density().   
#    ¿Logras identificar algún tipo de distribucion conocida?,¿Cuál o cuáles puedes
#    puedes identificar? Escribe el nombre de la variable y el tipo de distribución.
#    Si algunas te parecen desconocidas, escribe al lado "desconocida". (Exporta tus gráficas) 

## RESPUESTA
##  Miles per gallon- Binomial negativa, Number of cylinders- Bimodal
##  Displacement- Desconocida, Horse power-Hipergeométrica
##  Rear axle ratio-Desconocida,Weight (Lb)- Distribución Weibull,
##  Quater per mile time-Distribución de Poisson, Engine- Desconocida,
##  Transmission-DEsconocida, Number of foward gears-Desconocida,'
##  Number of carburetors- Desconocida.


### 1. Para los Histogramas

# Calling tidyverse
library(tidyr)

# Guardando el data frame mtcars con la etiqueta "autos"
autos= mtcars

# Declarando un vector con cada una de las variables del dataframe.
variables = c('Miles per gallon','Number of cylinders','Displacement',
              'Horse power','Rear axle ratio','Weight (Lb)',
              'Quater per mile time','Engine','Transmission',
              'Number of foward gears','Number of carburetors')

for(i in 1:length(variables)){
  png(paste("Histograma_", i, ".png"))
  hist(autos[,i], main = paste("Histogram_of_", variables[i]),
       xlab = "Model", ylab =variables[i])
  dev.off()
}



### 2. Para las distribuciones de probabilidad

# Calling tidyverse
library(tidyr)

# Guardando el data frame mtcars con la etiqueta "autos"
autos= mtcars

# Declarando un vector con cada una de las variables del dataframe.
variables = c('Miles per gallon','Number of cylinders','Displacement',
              'Horse power','Rear axle ratio','Weight (Lb)',
              'Quater per mile time','Engine','Transmission',
              'Number of foward gears','Number of carburetors')

for(i in 1:length(variables)){
  png(paste("Distribución_", i, ".png"))
  density(autos[,i], main = paste("Density_plot_of_", variables[i]),
       xlab = "Model", ylab =variables[i])
  dev.off()
}

### Manual

den1=density(autos[,1])
plot(den1, frame = FALSE, col = "blue",main = "Density plot Miles per gallon")

den2=density(autos[,2])
plot(den2, frame = FALSE, col = "blue",main = "Density plot Number of cylinders")


den3=density(autos[,3])
plot(den3, frame = FALSE, col = "blue",main = "Density plot Displacement")


den4=density(autos[,4])
plot(den4, frame = FALSE, col = "blue",main = "Density plot Horse power")


den5=density(autos[,5])
plot(den5, frame = FALSE, col = "blue",main = "Density plot Horse power")

### Segundo intento con cilos for

## intento de generar las distribuciones

# Calling tidyverse
library(tidyr)


# Declarando un vector con cada una de las variables del dataframe.
variables = c('Miles per gallon','Number of cylinders','Displacement',
              'Horse power','Rear axle ratio','Weight (Lb)',
              'Quater per mile time','Engine','Transmission',
              'Number of foward gears','Number of carburetors')
autos= mtcars
autos= as.data.frame(autos)

# Create a for loop to print and export 11 density plots
for (i in 1:11) {
  # Create a filename for the plot
  filename <- paste0("density_plot_", i, ".png")
  
  # Create the plot
  png(filename)
  plot(density(autos[,i]))
  dev.off()
}



# 5. Genera el resumen de estadística descriptiva de todas las variables, crea un dataframe 
#    con los resultados y exportalo como un archivo .csv llamado "Tabla Descriptiva.csv"

library(psych)
sm <- summary(mtcars)
print(sm)


## Este es el que sí usé y el que guardé

desc <- describe(mtcars)

resumen<-as.data.frame(desc)

write.csv(resumen, "Tabla Descriptiva.csv")

print(desc)



# 6. Realiza la prueba de normalidad a cada una de las variables y crea un dataframe 
#    con el nombre de la variable, el p-value y su categoría "normal" o "no normal".
#    Exporta el dataframe como un archivo .csv llamado "Prueba Normalidad.csv"

# instalando libreria
install.packages("nortest")
# Llamando a la libreria
library(nortest)
# Guardando de nuevo el dataframe mtcars como autos
autos<-mtcars
# Creando un data frame vacío que iré llenando confome se 
# ejecute el ciclo
resultados <- data.frame(matrix(ncol = 3,nrow = 0))
colnames(resultados)<-c("Column","P-value","Normal")
# Aplicación de la prueba de normalidad
# Shapiro test a cada una de las columnas data frame autos
# No funciona
for (i in 1:length(ncol(autos))){
  column <- autos[,i]
  test <- shapiro.test(column)
  normal <- ifelse(test$p.value >= 0.05, "Yes", "No")
  resultados <- rbind(resultados, c(colnames(autos[i]), test$p.value, normal))
} 
# Esta si funcionó!!!
# Aplicación de la prueba de normalidad
# Shapiro test a cada una de las columnas data frame autos
for (i in 1:11){
  column <- autos[,i]
  test <- shapiro.test(column)
  normal <- ifelse(test$p.value >= 0.05, "Yes", "No")
  resultados <- rbind(resultados, c(names(autos)[i], test$p.value, normal))
  #print(test)
}
# cmabiando los nombres de las columnas del nuevo dtaframe
colnames(resultados)<-c("Variable","P-value","Normal")
# Exportando el csv 

write.csv(resultados, "Prueba Normalidad.csv")



# 7. Guarda todo tu global environment en un archivo llamado "Ejercicio1.RData".




