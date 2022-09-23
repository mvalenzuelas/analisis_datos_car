library(dplyr)
library(tidyr)
library(scales)
library(leaps)
library(caret)
library(corrplot)
library(fastDummies)
setwd("C://Users//Maxi//Desktop//semestre8//analisis_datos")

#Cargar información sobre autos y almcenarlos en un dataframe cambiando el 
# nombre de las columnas
data<-read.csv("car.data",header = FALSE)
colnames(data)<-c("price", "maint", "doors", "persons", "lug_boot","safety","class")

#Modificar los tipos de datos a factor
data$price<-factor(data$price)
data$maint<-factor(data$maint)
data$doors<-factor(data$doors)
data$persons<-factor(data$persons)
data$lug_boot<-factor(data$lug_boot)
data$safety<-factor(data$safety)
data$class<-factor(data$class)

#Frecuencia de valores en class

grafico.barras.clase<-barplot(table(data$class), ylab = "Frecuencia",
                              main = "Frecuencia de distintas clasificaciones de vehiculos",
                              col = c("orange", "skyblue", "red" , "green")
                              )
par(mfrow = c(3, 2))
g1<-barplot(table(data$price), ylab = "Frecuencia",
            main = "Frecuencia de distintas precios de vehiculos",
            col = c("orange", "skyblue", "red" , "green")
            )
g2<-barplot(table(data$main), ylab = "Frecuencia",
            main = "Frecuencia de distintas precios de mantencion de vehiculos",
            col = c("orange", "skyblue", "red" , "green"))
g3<-barplot(table(data$doors), ylab = "Frecuencia",
            main = "Frecuencia de la cantidad de puertas de vehiculos",
            col = c("orange", "skyblue", "red" , "green"))
g4<-barplot(table(data$persons), ylab = "Frecuencia",
            main = "Frecuencia de la cantidad de pasajeros  de vehiculos",
            col = c("orange", "skyblue", "red"))
g5<-barplot(table(data$lug_boot), ylab = "Frecuencia",
            main = "Frecuencia de los tamaños de portaequipajes de vehiculos",
            col = c("orange", "skyblue", "red"))
g6<-barplot(table(data$safety), ylab = "Frecuencia",
        main = "Frecuencia del nivel de seguridad de vehiculos",
        col = c("orange", "skyblue", "red"))
par(mfrow = c(1, 1))




#Generar tablas de frecuencia y proporciones de las distintas variables con la 
# clase
tabla.precio.clase<-table(data$price,data$class)
tabla.precio.clase.prop<-paste("(",round(prop.table(tabla.precio.clase)*100,2),")%",sep='')
tabla.precio.clase.aux<-tabla.precio.clase
i=1
for (i in 1:16){
  tabla.precio.clase.aux[i]<-paste(tabla.precio.clase[i],tabla.precio.clase.prop[i])
}
print(tabla.precio.clase.prop)
mosaicplot(tabla.precio.clase,main="Proporcion Precios VS Clasificacion")


tabla.mantenimiento.clase<-table(data$maint,data$class)
tabla.mantenimiento.clase.prop<-paste("(",round(prop.table(tabla.mantenimiento.clase)*100,2),")%",sep='')
tabla.mantenimiento.clase.aux<-tabla.mantenimiento.clase
i=1
for (i in 1:16){
  tabla.mantenimiento.clase.aux[i]<-paste(tabla.mantenimiento.clase[i],tabla.mantenimiento.clase.prop[i])
}
print(tabla.mantenimiento.clase.prop)
mosaicplot(tabla.mantenimiento.clase,main="Proporcion Precio Mantenimiento VS Clasificacion")

tabla.personas.clase<-table(data$persons,data$class)
tabla.personas.clase.prop<-paste("(",round(prop.table(tabla.personas.clase)*100,2),")%",sep='')
i=1
tabla.personas.clase.aux<-tabla.personas.clase
for (i in 1:12){
  tabla.personas.clase.aux[i]<-paste(tabla.personas.clase[i],tabla.personas.clase.prop[i])
}
print(tabla.personas.clase.prop)
mosaicplot(tabla.personas.clase,main="Proporcion Cantidad Pasajeros VS Clasificacion")

tabla.puertas.clase<-table(data$doors,data$class)
tabla.puertas.clase.prop<-paste("(",round(prop.table(tabla.puertas.clase)*100,2),")%",sep='')
i=1
tabla.puertas.clase.aux<-tabla.puertas.clase
for (i in 1:12){
  tabla.puertas.clase.aux[i]<-paste(tabla.puertas.clase[i],tabla.puertas.clase.prop[i])
}
print(tabla.puertas.clase.prop)
mosaicplot(tabla.puertas.clase,main="Proporcion Cantidad Puertas VS Clasificacion")

tabla.equipaje.clase<-table(data$lug_boot,data$class)
tabla.equipaje.clase.prop<-paste("(",round(prop.table(tabla.equipaje.clase)*100,2),")%",sep='')
i=1
tabla.equipaje.clase.aux<-tabla.equipaje.clase
for (i in 1:12){
  tabla.equipaje.clase.aux[i]<-paste(tabla.equipaje.clase[i],tabla.equipaje.clase.prop[i])
}
print(tabla.equipaje.clase.prop)
mosaicplot(tabla.equipaje.clase,main="Proporcion Tamaño portaequipaje VS Clasificacion")

tabla.seguridad.clase<-table(data$safety,data$class)
tabla.seguridad.clase.prop<-paste("(",round(prop.table(tabla.seguridad.clase)*100,2),")%",sep='')
i=1
tabla.seguridad.clase.aux<-tabla.seguridad.clase
for (i in 1:12){
  tabla.seguridad.clase.aux[i]<-paste(tabla.seguridad.clase[i],tabla.seguridad.clase.prop[i])
}
print(tabla.seguridad.clase.prop)
mosaicplot(tabla.seguridad.clase,main="Proporcion Nivel seguridad VS Clasificacion")


#Escribir archivos con las tablas de frecuencias y proporciones
write.csv(tabla.precio.clase.aux,file = "precio.csv")

write.csv(tabla.mantenimiento.clase.aux,file = "mantencion.csv")

write.csv(tabla.puertas.clase.aux,file = "puertas.csv")

write.csv(tabla.personas.clase.aux,file = "personas.csv")

write.csv(tabla.equipaje.clase.aux,file = "equipaje.csv")

write.csv(tabla.seguridad.clase.aux,file = "seguridad.csv")


#Realizar prueba de inferencia no parametrica chi-squre de independencia para
# determinar si cada una de las variables es estadisticamente independiente de 
# la clase de seguridad
alfa<-0.05
cat("Prueba de independencia entre las variables clase y precio\n")
print(chisq.test(tabla.precio.clase))

cat("Prueba de independencia entre las variables clase y personas\n")
print(chisq.test(tabla.personas.clase))

cat("Prueba de independencia entre las variables lase y mantencion\n")
print(chisq.test(tabla.mantenimiento.clase))

cat("Prueba de independencia entre las variables clase y puertas\n")
print(chisq.test(tabla.puertas.clase))

cat("Prueba de independencia entre las variables clase y equipaje\n")
print(chisq.test(tabla.equipaje.clase))

cat("Prueba de independencia entre las variables clase y seguridad\n")
print(chisq.test(tabla.seguridad.clase))

#Modelo de regresión logistica
res <- cbind(model.matrix(class~price-1,data=data),
             model.matrix(class~maint-1,data=data),
             model.matrix(class~doors-1,data=data),
             model.matrix(class~persons-1,data=data),
             model.matrix(class~lug_boot-1,data=data),
             model.matrix(class~safety-1,data=data))
res<-as.data.frame(res)
correlacion<-round(cor(res,method="spearman"),1)
corrplot(correlacion,type="upper",method ="square",addCoef.col = "black")

muestra<-cbind(data$class,res)
colnames(muestra)[1]<-"class"
muestra$class<-factor(muestra$class,
                      levels = c("unacc","acc","good","vgood"),
                      labels = c(0,1,1,1))


n_entrenamiento<-sample.int(n=nrow(muestra),size=0.7*nrow(muestra),replace=FALSE)
muestra_entrenamiento<-muestra[n_entrenamiento,]
muestra_prueba<-muestra[-n_entrenamiento,]
modelos<-regsubsets (class ~ ., data = muestra , method = "exhaustive" ,
                      nbest = 1 , nvmax = 10)

predictores<-paste(names(which(summary(modelos)$which[8,-1]==TRUE)),collapse = "+")
formula<- as.formula(paste0("class", "~", predictores))

modelo<-glm(formula, family = binomial(link = "logit"),
            data = muestra_entrenamiento)
summary(modelo)


