rm(list = ls())

library(imager)
library(stringr)
# Set wd where images are located
setwd("C:\\Users\\LAPTOP\\Desktop\\I.A.A\\PROYECTO\\train")
images <- list.files() # Load images names
fpath<-"C:\\Users\\LAPTOP\\Desktop\\I.A.A\\PROYECTO\\train\\"

#Variables para la dimension de la imagen
height=300
width=300

images<-as.data.frame(images)
images = images[-203,] #En la ultima fila añade Thumbs ??, lo borro, hay q mirar por q lo añade

for(i in 1:length(images)){ 
  #Bucle para pasar por todas las imagenes de la lista
  #Aqui va todo el codigo
}




############  Codigo ejemplo con una sola imagen #############################################

####### UTILIZAMOS LA LIBRERIA IMAGER ########################################################

library(imager)
pet <- load.image(str_c(fpath,images[9])) # leer con libreria imager
pet<- resize(pet,height,width)
plot(pet,main="foto")
#Redimensionamos la imagen
value<-R(pet)
pet<- as.data.frame(pet)
pet <- plyr::mutate(pet,channel=factor(cc,labels=c('R','G','B')))

#Sacar valores RGB
#Color promedio de toda la imagen
mean_color_image<-mean(pet$value)

#Sacar valor Rojo promedio, sumar los primeros 40000 valores del canal 1 y sacar la media
red<-head(pet,40000)
mean_red<-mean(red$value)
#Sacar valor Verde promedio, sumar los valores entre 40000 y 80000 del canal 2 y sacar la media
green<-subset(pet,cc==2)
mean_green<-mean(green$value)
#Sacar valor Azul promedio, sumar los ultimos 40000 valores del canal 3 y sacar la media
blue<-tail(pet,40000)
mean_blue<-mean(blue$value)

#Sacar la luminosidad
pet <- load.image(str_c(fpath,images[10])) # leer con libreria imager  58
pet<- resize(pet,height,width)

grayimage=grayscale(pet)
plot(grayscale(pet))
pet<- as.data.frame(pet)
mean_color_grayimage<-mean(pet$value)
#La media nos da lo mismo que con la imagen en color, PREGUNTAR PORQUE

###########  Image Gradient ####################################################
pet <- load.image(str_c(fpath,images[10])) # leer con libreria imager
pet<- resize(pet,height,width)
gradient<-imgradient(pet,"xy")
plot(gradient)
gradient<-as.data.frame(gradient)

#Sacar el valor promedio del gradiente, todos los canales
subset_gradient<-subset(gradient,gradient$value>=0)
mean_gradient<-mean(subset_gradient$value)

#Sacar el valor promedio del gradiente en el canal 1 (red)
subset_gradient_channel1<-subset(subset_gradient, subset_gradient$cc==1)
mean_gradient_channel1<-mean(subset_gradient_channel1$value)
#Sacar el valor promedio del gradiente en el canal 2 (green)
subset_gradient_channel2<-subset(subset_gradient, subset_gradient$cc==2)
mean_gradient_channel2<-mean(subset_gradient_channel2$value)
#Sacar el valor promedio del gradiente en el canal 3 (blue)
subset_gradient_channel3<-subset(subset_gradient, subset_gradient$cc==3)
mean_gradient_channel3<-mean(subset_gradient_channel3$value)


################## Determinant of Gesian ###################################################

#Detectar objetos pequeños
get.centers <- function(im,thr="99%"){
  dt <- imhessian(im) %$% { xx*yy - xy^2 } %>% threshold(thr) %>% label
  as.data.frame(dt) %>% subset(value>0) %>% dplyr::group_by(value) %>% dplyr::summarise(mx=mean(x),my=mean(y))
}

plot(grayimage)
get.centers(grayimage,"99%") %$% points(mx,my,col="red")

#Detectar objetos de diferente escala
#Compute determinant at scale "scale". 
hessdet <- function(im,scale=1) isoblur(im,scale) %>% imhessian %$% { scale^2*(xx*yy - xy^2) }
#Note the scaling (scale^2) factor in the determinant
result_scale=as.data.frame(hessdet(grayimage,1))
plot(hessdet(grayimage,1),main="Determinant of the Hessian at scale 1")
get.centers(grayimage,"98.5%") %$% points(mx,my,col="red")

prueba<-as.data.frame(get.centers(grayimage,"98.5%"))







  
  

