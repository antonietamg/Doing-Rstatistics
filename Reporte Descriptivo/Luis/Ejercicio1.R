autos = mtcars
save(autos, file = "autos.RData")

Test = function(D, ruta){
  library(psych)
  i = 1
  num_columnas = ncol(D)
  colors = rainbow(num_columnas)
  # Dataframe para la tabla descriptiva
  descripciones_df = data.frame(Columna = character(0), Descripcion = character(0))
  # Dataframe para la prueba de normalidad
  normalidad_df = data.frame(Columna = character(0), Descripcion = character(0), Normalidad = character(0))
  # Dataframe para la tabla de medidas de tendencia central
  tendencia_central_df = data.frame(Columna = character(0), Media = numeric(0), Mediana = numeric(0))
  
  # Vector con los nombres de las carpetas a crear
  carpetas = c("Density_graf", "Hist_graf", "Line_graf")
  
  # Ciclo para crear las carpetas
  for (carpeta in carpetas) {
    ruta_carpeta <- file.path(ruta, carpeta)
    
    # Verificar si la carpeta ya existe
    if (!dir.exists(ruta_carpeta)) {
      # Si la carpeta no existe, entonces la creamos
      dir.create(ruta_carpeta)
      cat("La carpeta se ha creado con éxito en la ruta:", ruta_carpeta, "\n")
    } else {
      # Si la carpeta ya existe, simplemente informamos que ya existe
      cat("La carpeta ya existe en la ruta:", ruta_carpeta, "\n")
    }
  }
  

  
  while (i <= num_columnas) {
    # Nombres de las gráficas
    M_label_lines = paste("Gráfico de líneas de ", colnames(D)[i])
    M_label_histogram = paste("Histograma de ", colnames(D)[i])
    M_label_density = paste("Density de ", colnames(D)[i])
    
    # Gráfico de líneas
    png(file = paste0(ruta,"Line_graf/", "grafica_", i, ".png"))
    plot(D[, i], type = "l", col = colors[i], xlab = "Index", ylab = colnames(D)[i], main = M_label_lines)
    dev.off()
    
    # Histograma
    png(file = paste0(ruta,"Hist_graf/", "histograma_", i, ".png"))
    hist(D[, i], col = colors[i], xlab = "Index", ylab = colnames(D)[i], main = M_label_histogram)
    dev.off()
    
    # Gráfico de densidad
    png(file = paste0(ruta,"Density_graf/", "density_", i, ".png"))
    plot(density(D[, i]), col = colors[i], xlab = "Index", ylab = colnames(D)[i], main = M_label_density)
    dev.off()
    
    # Tabla descriptiva
    columna = D[, i]
    descripcion = describe(columna)
    descripciones_df = rbind(descripciones_df, data.frame(Columna = colnames(D)[i], Descripcion = capture.output(descripcion), Normalidad = NA))
    
    # Prueba de normalidad
    normalidad = shapiro.test(columna)
    descripcion_normalidad = capture.output(normalidad)
    es_normal = ifelse(normalidad$p.value > 0.05, "Normal", "No Normal")
    normalidad_df = rbind(normalidad_df, data.frame(Columna = colnames(D)[i], Descripcion = descripcion_normalidad, Normalidad = es_normal))
    
    #Medidas de tendencia central
    media = mean(columna)
    mediana = median(columna)
    tendencia_central_df = rbind(tendencia_central_df, data.frame(Columna = colnames(D)[i], Media = media, Mediana = mediana))
    
    i = i + 1
  }
  
  # Exportar los resultados a CSV
  write.csv(descripciones_df, file = paste(ruta, "Tabla_descriptiva.csv"), row.names = FALSE)
  write.csv(normalidad_df, file = paste(ruta, "Prueba_Normalidad.csv"), row.names = FALSE)
  write.csv(tendencia_central_df, file = paste(ruta, "Medidas_Tendencia_Central.csv"), row.names = FALSE)
  
}

Test(autos, "/home/l_f_caballero/Documentos/Luis/")

save.image(file = "/home/l_f_caballero/Documentos/Luis/Ejercicio1.RData")
