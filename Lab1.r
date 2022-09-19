library(dplyr)
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


#Generar tablas de frecuencia y proporciones de las distintas variables con la 
# clase
tabla.precio.clase<-table(data$price,data$class)
print(tabla.precio.clase)
print(prop.table(tabla.precio.clase))

tabla.matencion.clase<-table(data$maint,data$class)
print(tabla.mantencion.clase)
print(prop.table(tabla.mantencion.clase))

tabla.puertas.clase<-table(data$doors,data$class)
print(tabla.puertas.clase)
print(prop.table(tabla.puertas.clase))

tabla.personas.clase<-table(data$persons,data$class)
print(tabla.personas.clase)
print(prop.table(tabla.personas.clase))

tabla.equipaje.clase<-table(data$lug_boot,data$class)
print(tabla.equipaje.clase)
print(prop.table(tabla.equipaje.clase))

tabla.seguridad.clase<-table(data$safety,data$class)
print(tabla.seguridad.clase)
print(prop.table(tabla.seguridad.clase))


#Escribir archivos con las tablas de frecuencias y proporciones
write.csv2(cbind(tabla.precio.clase,prop.table(tabla.precio.clase)),file = "precio.csv")
write.csv2(cbind(tabla.mantencion.clase,prop.table(tabla.mantencion.clase)),file = "mantencion.csv")
write.csv2(cbind(tabla.personas.clase,prop.table(tabla.personas.clase)),file = "personas.csv")
write.csv2(cbind(tabla.puertas.clase,prop.table(tabla.puertas.clase)),file = "puertas.csv")
write.csv2(cbind(tabla.equipaje.clase,prop.table(tabla.equipaje.clase)),file = "equipaje.csv")
write.csv2(cbind(tabla.seguridad.clase,prop.table(tabla.seguridad.clase)),file = "seguridad.csv")


#Realizar prueba de inferencia no parametrica chi-squre de independencia para
# determinar si cada una de las variables es estadisticamente independiente de 
# la clase de seguridad
alfa<-0.05
cat("Prueba de independencia entre las variables clase y precio")
chisq.test(tabla.precio.clase)

cat("Prueba de independencia entre las variables clase y personas")
chisq.test(tabla.personas.clase)

cat("Prueba de independencia entre las variables lase y mantencion")
chisq.test(tabla.mantencion.clase)

cat("Prueba de independencia entre las variables clase y puertas")
chisq.test(tabla.puertas.clase)

cat("Prueba de independencia entre las variables clase y equipaje")
chisq.test(tabla.equipaje.clase)

cat("Prueba de independencia entre las variables clase y seguridad")
chisq.test(tabla.seguridad.clase)




