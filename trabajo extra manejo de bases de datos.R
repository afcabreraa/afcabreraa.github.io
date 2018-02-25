setwd("C:/Users/ANDRES FELIPE/Desktop/andres/univesidad nacional/semestre II/exploratoria/trabajo extra EDAED")
set.seed(1023472348)
sort(sample(1:20, 4))

#lectura bases de datos
grupo01 <- read.csv2("grupo01.csv", header = TRUE)
grupo02 <- read.csv2("grupo02.csv", header = TRUE)
grupo15 <- read.csv2("grupo15.csv", header = TRUE)
grupo19 <- read.csv2("grupo19.csv", header = TRUE)

#variables bases de datos
names(grupo01)
names(grupo02)
names(grupo15)
names(grupo19)


#union de las bases de datos
basetotal <- rbind(grupo01, grupo02,grupo15,grupo19)

#analisis base todal de datos
names(basetotal)
summary(basetotal)

#se analizara las variables genero,edad,altura,ancho mano,si practica o no deporte.
summary(basetotal$Genero)
summary(basetotal$Edad)
summary(basetotal$Estatura)
summary(basetotal$AnchoMano)
summary(basetotal$Deporte)

#grafica boxplot de las variables a analizar

#genero
masculino <- subset(basetotal,Genero == "Masculino",select = c("Genero","TmpReac3"))
femenino <- subset(basetotal,Genero == "Femenino",select = c("Genero","TmpReac3"))
with(masculino, boxplot(TmpReac3,ylim=c(0,800),xlab="Masculino"))
with(femenino,  boxplot(TmpReac3,ylim=c(0,800),xlab="Femenino"))

#edad
edad1 <- subset(basetotal,Edad<26 & Edad>=16 ,select = c("Edad","TmpReac3"))    
edad2 <- subset(basetotal,Edad<36 & Edad>=26 ,select = c("Edad","TmpReac3"))
edad3 <- subset(basetotal,Edad<46 & Edad>=36 ,select = c("Edad","TmpReac3"))
edad4 <- subset(basetotal,Edad<56 & Edad>=46 ,select = c("Edad","TmpReac3"))
with(edad1, boxplot(TmpReac3,ylim=c(0,800),xlab="16 a?os-25 a?os"))
with(edad2, boxplot(TmpReac3,ylim=c(0,800),xlab="26 a?os-35 a?os"))
with(edad3, boxplot(TmpReac3,ylim=c(0,800),xlab="36 a?os-45 a?os"))
with(edad4, boxplot(TmpReac3,ylim=c(0,800),xlab="46 a?os-56 a?os"))

#ancho mano
mano1 <- subset(basetotal,AnchoMano<9 & AnchoMano>=6 ,select = c("AnchoMano","TmpReac3"))    
mano2 <- subset(basetotal,AnchoMano<12 & AnchoMano>=9 ,select = c("AnchoMano","TmpReac3"))
mano3 <- subset(basetotal,AnchoMano<=15 & AnchoMano>=12 ,select = c("AnchoMano","TmpReac3"))
with(mano1, boxplot(TmpReac3,ylim=c(0,800),xlab="[6 cm-9 cm)"))
with(mano2, boxplot(TmpReac3,ylim=c(0,800),xlab="[9 cm-12 cm)"))
with(mano3, boxplot(TmpReac3,ylim=c(0,800),xlab="[12 cm-15 cm]"))

#deporte
deportesi <- subset(basetotal,Deporte == "si",select = c("Deporte","TmpReac3"))
deporteno <- subset(basetotal,Deporte == "no",select = c("Deporte","TmpReac3"))
with(deportesi, boxplot(TmpReac3,ylim=c(0,800),xlab="practica deporte"))
with(deporteno, boxplot(TmpReac3,ylim=c(0,800),xlab="no practica deporte"))

#videojuegos
vjuego1 <- subset(basetotal,HorasVideoJuego<10 & HorasVideoJuego>=0 ,select = c("HorasVideoJuego","TmpReac3"))    
vjuego2 <- subset(basetotal,HorasVideoJuego<20 & HorasVideoJuego>=10 ,select = c("HorasVideoJuego","TmpReac3")) 
vjuego3 <- subset(basetotal,HorasVideoJuego<=30 & HorasVideoJuego>=20 ,select = c("HorasVideoJuego","TmpReac3")) 
with(vjuego1, boxplot(TmpReac3,ylim=c(0,800),xlab="[0 - 10) horas de juego"))
with(vjuego2, boxplot(TmpReac3,ylim=c(0,800),xlab="[10 - 20) horas de juego"))
with(vjuego3, boxplot(TmpReac3,ylim=c(0,800),xlab="[20 - 30] horas de juego"))
