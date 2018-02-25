hurtbogo <- read.csv2("base de datos de hurtos de motocicletas.csv")
hurtbogo
barrios <- c("Kennedy","Engativá","Ciudad Bolivar","Bosa","Rafael Uribe","San Cristobal","Puente Aranda","Suba","Tunjuelito","Fontibón","Barrios Unidos","Usme","Teusaquillo","Antonio Nariño","Los Mártires","Usaquén","Chapinero","Santa Fe","Candelaria")

hurtbogo$Localidad <- factor(hurtbogo$Localidad, levels = barrios)

with(hurtbogo , plot(1:length(Localidad), X2014, 
                     type = "b",
                     col = "red", las = 2,
                     lwd = 0.5,
                     ylab = "",
                     xlab = "",
                     axes = FALSE,
                     main = "motos hurtadas en las localidades de Bogotá"))
par(new=TRUE)
with(hurtbogo , plot(1:length(Localidad), X2015,
                     ylim = c(0,500),
                     yaxp = c(0,500,10),
                     type = "b",
                     col = "blue",
                     lwd = 0.5,
                     xlab = "",
                     xaxt = "n",
                     ylab = "cantidad de motos",
                     las = 2))
text(seq(1, 19, by=1), par("usr")[1] - 60,
     labels = barrios, srt = 45, pos = 1, xpd = TRUE)
grid()
color <- c("red","blue")
legend("topright",c("2014","2015"),cex = 0.9, fill = color)