#Se establece el PATH donde se guarda el script R
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
datos_train=read.csv("DatasetTrain.csv")
datos_train[1]<-NULL
datos_test=read.csv("DatasetTest.csv")
datos_test[1:2]<-NULL
datos_test$animaltype=0

library(neuralnet)
# RED NEURONAL
# -----------------------------------------------------
nms  <- names(datos_train[3:15])
frml <- as.formula(paste("animaltype ~", paste(nms, collapse = " + ")))

# VARIABLES DE CONFIGURACION
# -----------------------------------------------------
numeroCapasOcultas=c(20,10,5)
thres=0.05

# MODELO
# -----------------------------------------------------
modelo <- neuralnet(frml,
                    data = datos_train,
                    hidden = numeroCapasOcultas,
                    rep = 1, #numero de iteraciones
                    lifesign = "full",
                    linear.output = FALSE,
                    threshold     = thres,
                    algorithm     = "rprop+")

#GRAFICO DEL MODELO
# -----------------------------------------------------
plot(modelo,rep = "best") #best muestra el mejor de todas las iteracciones




#Mirar desde aqui............................



# PREDICCION
# -----------------------------------------------------
prediccion  <- compute(modelo,within(datos_test,rm(animaltype)))
tabla<-data.frame(Real = datos_test$animaltype, Predicted = prediccion$net.result, Error = abs(datos_test$animaltype - prediccion$net.result) / datos_test$animaltype)

# se transforma el valor escalar al valor nominal original
#strength.predict <- prediccion$net.result*(max(datos$strength)-min(datos$strength))+min(datos$strength)
#strength.real    <- (datos_test$strength)*(max(datos$strength)-min(datos$strength))+min(datos$strength)

# SUMA DE ERROR CUADRATICO
# -----------------------------------------------------
#(se.nn <- sum((strength.real - strength.predict)^2)/nrow(datos_test))

#GRAFICO ERRORES
# -----------------------------------------------------
#qplot(x=strength.real, y=strength.predict, geom=c("point","smooth"), method="lm", 
      #main=paste("Real Vs Prediccion. Summa de Error Cuadratico=", round(se.nn,2)))


#MEDIR LA CORRELACION ENTRE LOS RESULTADOS DE LA RED
# -----------------------------------------------------
#prediccion.strength <- prediccion$net.result
#cor(prediccion.strength,datos_test$animaltype)




